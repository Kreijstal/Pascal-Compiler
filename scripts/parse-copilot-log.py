#!/usr/bin/env python3
"""Parse Copilot Agent GH Actions logs into a complete AST.

Every line in the log becomes a node — nothing is discarded.

AST shape:
  RootNode
  ├── AgentNode
  │    ├── ThinkingNode(text)
  │    └── ToolCallNode(name, args, result)
  └── RunnerNode(text)       ← setup, env vars, apt, etc. (not AI-generated)

Indentation defines scope. Content is never scanned for keywords.

Usage:
    python3 scripts/parse-copilot-log.py --file raw.log
    python3 scripts/parse-copilot-log.py <run_id> [--repo owner/repo]
    python3 -m parse_copilot_log <run_id> --json
    python3 -m parse_copilot_log <run_id> -vv
    python3 -m parse_copilot_log <run_id> --only-agent    # show only AI stuff
"""

import argparse, json, re, subprocess, sys
from dataclasses import dataclass, field
from typing import Union


# ── AST Nodes ────────────────────────────────────────────────────────────────

@dataclass
class Node:
    start_line: int = 0
    end_line: int = 0


@dataclass
class RunnerNode(Node):
    """Runner output — env vars, apt, checkout log, etc. Not AI-generated."""
    text: str = ""


@dataclass
class ThinkingNode(Node):
    text: str = ""


@dataclass
class NameNode(Node):
    tool_name: str = ""


@dataclass
class ArgsNode(Node):
    pairs: list[tuple[str, str]] = field(default_factory=list)

    def as_dict(self) -> dict:
        return dict(self.pairs)


@dataclass
class ResultNode(Node):
    text: str = ""


@dataclass
class ToolCallNode(Node):
    name: NameNode = field(default_factory=NameNode)
    args: ArgsNode = field(default_factory=ArgsNode)
    result: ResultNode = field(default_factory=ResultNode)


@dataclass
class AgentNode(Node):
    """A thinking → tool-call group."""
    thinking: ThinkingNode = field(default_factory=ThinkingNode)
    tool_calls: list[ToolCallNode] = field(default_factory=list)


@dataclass
class RootNode(Node):
    children: list[Union[RunnerNode, AgentNode]] = field(default_factory=list)


# ── Pre-processed line ───────────────────────────────────────────────────────

@dataclass
class Line:
    indent: int
    text: str


def preprocess(text: str) -> list[Line]:
    """Strip GH log prefix → [Line(indent, content)]."""
    out: list[Line] = []
    for raw in text.splitlines():
        m = re.match(r'^[^\t]*\t[^\t]*\t\S+ (.*)', raw)
        content = m.group(1) if m else raw
        indent = len(content) - len(content.lstrip(' '))
        out.append(Line(indent=indent, text=content.strip()))
    return out


# ── Parser helpers ───────────────────────────────────────────────────────────

def _collect_text(lines: list[Line], i: int) -> tuple[str, int]:
    """Collect consecutive lines with indent==0 into one text block.
    Returns (text, lines_consumed)."""
    parts: list[str] = []
    start = i
    while i < len(lines) and lines[i].indent == 0:
        t = lines[i].text
        # Skip blank lines that just separate chunks
        if not t and not parts:
            i += 1
            continue
        parts.append(t)
        i += 1
    return "\n".join(parts), i - start


def _build_args(lines: list[Line], i: int) -> tuple[ArgsNode, int]:
    """Parse 4-space indented key: value lines."""
    args = ArgsNode()
    pairs: list[tuple[str, str]] = []
    key: str | None = None
    val: list[str] = []

    while i < len(lines):
        ln = lines[i]
        if ln.indent < 4:
            break
        m = re.match(r'^(\w[\w_]*?)\s*:\s*(.*)', ln.text)
        if m and ln.indent == 4:
            if key:
                pairs.append((key, "\n".join(val).strip()))
            key, val = m.group(1), [m.group(2).strip()] if m.group(2).strip() else []
        elif key and ln.indent >= 4:
            if ln.text.strip():
                val.append(ln.text.strip())
        i += 1
    if key:
        pairs.append((key, "\n".join(val).strip()))
    args.pairs = pairs
    return args, i - start   # but wait, we haven't updated i — let me fix


def build_args(lines: list[Line], i: int) -> tuple[ArgsNode, int]:
    """Parse 4-space indented key: value pairs starting at line i.
    Returns (ArgsNode, lines_consumed_from_i)."""
    pairs: list[tuple[str, str]] = []
    key: str | None = None
    val: list[str] = []
    consumed = 0

    while (i + consumed) < len(lines):
        ln = lines[i + consumed]
        if ln.indent < 4:
            break
        m = re.match(r'^(\w[\w_]*?)\s*:\s*(.*)', ln.text)
        if m and ln.indent == 4:
            if key:
                pairs.append((key, "\n".join(val).strip()))
            key = m.group(1)
            val = [m.group(2).strip()] if m.group(2).strip() else []
        elif key and ln.indent >= 4:
            if ln.text.strip():
                val.append(ln.text.strip())
        consumed += 1

    if key:
        pairs.append((key, "\n".join(val).strip()))
    return ArgsNode(pairs=pairs), consumed


def build_result(lines: list[Line], i: int) -> tuple[ResultNode, int]:
    """Consume all lines with indent > 2 (result content)."""
    parts: list[str] = []
    consumed = 0
    while (i + consumed) < len(lines):
        ln = lines[i + consumed]
        if ln.indent == 0:
            break           # back to root level
        if ln.indent == 2:
            break           # another tool metadata
        if ln.text.startswith("<exited"):
            consumed += 1
            break
        parts.append(ln.text)
        consumed += 1
    return ResultNode(text="\n".join(parts)), consumed


def parse(lines: list[Line]) -> RootNode:
    root = RootNode()
    i = 0

    while i < len(lines):
        ln = lines[i]

        # ─ Runner noise at indent 0 (not AI) ──────────────────────────
        if ln.indent == 0 and not ln.text.startswith("copilot:") and ln.text != "function:":
            text = ""
            start = i
            while i < len(lines) and lines[i].indent == 0 \
                  and not lines[i].text.startswith("copilot:") \
                  and lines[i].text != "function:":
                t = lines[i].text
                if t or text:           # skip leading blanks only
                    text += ("\n" if text else "") + t
                i += 1
            if text.strip():
                root.children.append(RunnerNode(
                    text=text.strip(), start_line=start, end_line=i))
            continue

        # ─ AI: function: at indent 0 ──────────────────────────────────
        if ln.indent == 0 and ln.text == "function:":
            # Collect preceding thinking at indent 0
            thinking_parts: list[str] = []
            start_think = i
            while start_think > 0 and lines[start_think - 1].indent == 0 \
                  and lines[start_think - 1].text.startswith("copilot:"):
                start_think -= 1
            for j in range(start_think, i):
                body = lines[j].text[len("copilot:"):].strip()
                if body:
                    thinking_parts.append(body)
            thinking_text = "\n".join(thinking_parts).strip()

            # Remove thinking lines from queue (they're consumed)
            # Actually they're already before i, so we just need to not re-read them
            # The thinking lines are at indices [start_think..i-1], all indent 0 copilot:
            # We track them via the AgentNode we're building.

            # Build the ToolCallNode
            tool_start = i
            tool, consumed = _build_tool(lines, i)
            tool.start_line = tool_start
            # Find the last thinking line consumed
            agent_start = start_think

            # Remove the thinking lines from already-processed children if any
            # Actually, thinking lines are NOT yet in root.children — we haven't
            # added anything for them yet. They're just in lines[].
            # We need to skip them in the outer loop.
            # But the outer loop processes lines[i] sequentially, and thinking
            # is *before* the function: line, so we need to walk backward to
            # collect them, then forward-skip them? No — let's restructure.

            # Simpler: build the tool, then create AgentNode.
            # But the outer loop is already at i=function:. The thinking is
            # at indices < i. The outer loop will never see them again.
            # So we MUST collect them here.
            # Then we must not double-process them — done, since we just added
            # them to this AgentNode.

            agent = AgentNode(
                thinking=ThinkingNode(
                    text=thinking_text,
                    start_line=start_think, end_line=i),
                tool_calls=[tool],
                start_line=start_think,
                end_line=i + consumed,
            )
            root.children.append(agent)
            i += consumed
            continue

        # ─ AI: copilot: thinking NOT followed by function: ────────────
        if ln.indent == 0 and ln.text.startswith("copilot:"):
            parts: list[str] = []
            start = i
            while i < len(lines) and lines[i].indent == 0 \
                  and lines[i].text.startswith("copilot:"):
                body = lines[i].text[len("copilot:"):].strip()
                if body:
                    parts.append(body)
                i += 1
            root.children.append(AgentNode(
                thinking=ThinkingNode(
                    text="\n".join(parts).strip(),
                    start_line=start, end_line=i),
                tool_calls=[],
                start_line=start, end_line=i,
            ))
            continue

        # ─ Non-zero indent outside any known structure (shouldn't happen with
        #    well-formed GH logs, but handle gracefully) ──────────────────
        i += 1

    root.end_line = len(lines)
    return root


def _build_tool(lines: list[Line], i: int) -> tuple[ToolCallNode, int]:
    """Parse from 'function:' line to end of tool call."""
    tool = ToolCallNode()
    consumed = 0   # lines consumed FROM i (starting at 'function:')
    consumed += 1  # skip 'function:'
    i += 1

    # name:
    while i < len(lines):
        if lines[i].indent == 2 and lines[i].text.startswith("name: "):
            tool.name = NameNode(
                tool_name=lines[i].text[len("name: "):].strip(),
                start_line=i, end_line=i)
            i += 1; consumed += 1
            break
        if lines[i].indent < 2:
            break
        i += 1; consumed += 1

    # args:
    while i < len(lines) and lines[i].indent == 2 and lines[i].text == "args:":
        args_node, ac = build_args(lines, i + 1)
        tool.args = args_node
        tool.args.start_line = i
        tool.args.end_line = i + ac
        i += 1 + ac; consumed += 1 + ac
        break
    else:
        while i < len(lines) and lines[i].indent > 0 and \
              not (lines[i].indent == 2 and lines[i].text.startswith("result:")):
            if lines[i].indent == 0:
                break
            i += 1; consumed += 1

    # result:
    while i < len(lines):
        ln = lines[i]
        if ln.indent == 2 and ln.text.startswith("result:"):
            rnode, rc = build_result(lines, i + 1)
            tool.result = rnode
            tool.result.start_line = i
            tool.result.end_line = i + 1 + rc
            i += 1 + rc; consumed += 1 + rc
            break
        if ln.indent == 0:
            break
        i += 1; consumed += 1

    tool.end_line = len(lines)  # approximate
    return tool, consumed


# ── Render ───────────────────────────────────────────────────────────────────

def render(root: RootNode, only_agent: bool = False, max_result: int = 15):
    for child in root.children:
        if isinstance(child, RunnerNode):
            if not only_agent:
                lines = child.text.splitlines()
                print(f"── Runner ({child.end_line - child.start_line} lines) ──")
                for ln in lines[:3]:
                    print(f"  ⚙️ {ln}")
                if len(lines) > 3:
                    print(f"  ⚙️ … ({len(lines) - 3} more)")
                print()

        elif isinstance(child, AgentNode):
            if child.thinking.text:
                print(f"── Agent Block {child.start_line}-{child.end_line} ──")
                for ln in child.thinking.text.splitlines():
                    print(f"  💭 {ln}")
                print()

            for tool in child.tool_calls:
                desc = dict(tool.args.pairs).get("description", "")
                print(f"  🔧 {tool.name.tool_name}" + (f"  📋 {desc}" if desc else ""))
                for k, v in tool.args.pairs:
                    if k == "description":
                        continue
                    preview = (v.splitlines()[0] if v else "")[:120]
                    print(f"     {k}: {preview}")
                print()
                if tool.result.text:
                    rlines = tool.result.text.splitlines()
                    for ln in rlines[:max_result]:
                        print(f"     │ {ln}")
                    if len(rlines) > max_result:
                        print(f"     │ … ({len(rlines) - max_result} more)")
                print()

    # Summary
    runner_bytes = sum(len(c.text) for c in root.children if isinstance(c, RunnerNode))
    agent_count = sum(len(c.tool_calls) for c in root.children if isinstance(c, AgentNode))
    thinking_count = sum(1 for c in root.children if isinstance(c, AgentNode) and c.thinking.text)
    tool_counts: dict[str, int] = {}
    for c in root.children:
        if isinstance(c, AgentNode):
            for t in c.tool_calls:
                tool_counts.setdefault(t.name.tool_name, 0)
                tool_counts[t.name.tool_name] += 1

    print("=" * 50)
    print(f"Runner blocks: {sum(1 for c in root.children if isinstance(c, RunnerNode))}")
    print(f"  bytes: {runner_bytes}")
    print(f"Agent blocks: {sum(1 for c in root.children if isinstance(c, AgentNode))}")
    print(f"  with thinking: {thinking_count}")
    print(f"  tool calls: {agent_count}")
    for name in sorted(tool_counts, key=tool_counts.get, reverse=True):
        print(f"  {name}: {tool_counts[name]}")


# ── I/O ──────────────────────────────────────────────────────────────────────

def download_log(repo: str, run_id: str) -> str:
    job_id = subprocess.run(
        ["gh", "run", "view", run_id, "--repo", repo,
         "--jq", ".jobs[0].databaseId"],
        capture_output=True, text=True,
    ).stdout.strip()
    if not job_id:
        sys.exit(f"No jobs for run {run_id}")
    print(f"Downloading job {job_id} …", file=sys.stderr)
    r = subprocess.run(
        ["gh", "run", "view", "--repo", repo, f"--job={job_id}", "--log"],
        capture_output=True, text=True,
    )
    return r.stdout if r.stdout.strip() else sys.exit("Empty log")


# ── CLI ──────────────────────────────────────────────────────────────────────

def main():
    p = argparse.ArgumentParser()
    p.add_argument("run_id", nargs="?")
    p.add_argument("--file")
    p.add_argument("--repo", default="Kreijstal/Pascal-Compiler")
    p.add_argument("--json", action="store_true")
    p.add_argument("-vv", "--verbose", action="store_true")
    p.add_argument("--only-agent", action="store_true",
                   help="Hide RunnerNode output")
    a = p.parse_args()

    raw = (open(a.file).read() if a.file
           else download_log(a.repo, a.run_id) if a.run_id
           else p.print_help() or sys.exit())

    root = parse(preprocess(raw))

    if a.json:
        def _to_dict(node):
            d = {"type": node.__class__.__name__}
            for f_name in getattr(node, '__dataclass_fields__', {}):
                v = getattr(node, f_name)
                if isinstance(v, Node):
                    d[f_name] = _to_dict(v)
                elif isinstance(v, list):
                    if v and isinstance(v[0], Node):
                        d[f_name] = [_to_dict(c) for c in v]
                    else:
                        d[f_name] = v
                elif f_name not in ('start_line', 'end_line') and v:
                    d[f_name] = v
            return d
        print(json.dumps(_to_dict(root), indent=2))
    else:
        render(root, only_agent=a.only_agent,
               max_result=50 if a.verbose else 15)

if __name__ == "__main__":
    main()
