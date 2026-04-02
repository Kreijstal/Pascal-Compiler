#!/usr/bin/env python3
"""
Python reimplementation of FPC's msg2inc utility (M_String mode).
Generates msgidx.inc and msgtxt.inc from a .msg file.

This is used to generate the message include files required by the FPC
compiler sources (verbose.pas -> cmsgs.pas). The original msg2inc is a
Pascal program (FPCSource/compiler/utils/msg2inc.pp) that requires a
working FPC compiler to build, so we provide this Python equivalent.

Usage: python3 msg2inc.py <msgfile> <outbase> <constname>
Example: python3 msg2inc.py msg/errore.msg msg msg
  -> generates msgidx.inc and msgtxt.inc
"""

import sys
import os

MSGPARTS = 20
MAXSLEN = 240  # max string length per array element


def load_msg_file(filename):
    """
    Parse the .msg file following the logic of msg2inc.pp LoadMsgFile.

    Returns (msgtxt: bytes, enum_entries: list[(name, number)],
             msgidxmax: list[int], codepage: int)

    The msgtxt buffer format:
    - For single-line entries: text from after '=' to end of line,
      terminated by NUL
    - For multiline entries: number prefix through '[', then each content
      line separated by LF, final LF replaced by NUL
    """
    with open(filename, "r", encoding="latin-1") as f:
        lines = f.readlines()

    codepage = 0  # CP_ACP default
    msgidxmax = [0] * (MSGPARTS + 1)  # 1-indexed (0 unused)
    msgs = {}  # (part, idx) -> True, for duplicate detection

    enum_parts = []  # list of (name, number_str)
    msgtxt_parts = []  # list of bytes

    multiline = False
    current_multi_bytes = bytearray()
    line_num = 0

    for raw_line in lines:
        line_num += 1
        s = raw_line.rstrip("\n").rstrip("\r")

        if multiline:
            if s == "]":
                multiline = False
                # Overwrite last LF with NUL
                if current_multi_bytes and current_multi_bytes[-1] == 0x0A:
                    current_multi_bytes[-1] = 0x00
                else:
                    current_multi_bytes.append(0x00)
                msgtxt_parts.append(bytes(current_multi_bytes))
            elif s == "" or s[0] != "#":
                current_multi_bytes.extend(s.encode("latin-1"))
                current_multi_bytes.append(0x0A)
            continue

        if s == "" or s[0] in ("#", ";", "%"):
            if len(s) > 11 and s[:11] == "# CodePage ":
                try:
                    codepage = int(s[11:].strip())
                except ValueError:
                    print(
                        f"Error in line {line_num}: illegal code page: {s}",
                        file=sys.stderr,
                    )
            continue

        # Regular message entry: name=NNNNN_T_text
        i = s.find("=")
        if i < 0:
            continue

        # Extract 5-digit number starting right after '='
        j = i + 1
        while j < len(s) and s[j].isdigit():
            j += 1

        num_len = j - i - 1
        if num_len != 5:
            # Not a message entry (option help text like **1b_...)
            continue

        number_str = s[i + 1 : j]
        num = int(number_str)
        numpart = num // 1000
        numidx = num % 1000

        if numpart == 0:
            print(
                f"Error in line {line_num}: number should be > 1000",
                file=sys.stderr,
            )
            continue
        if numpart > MSGPARTS:
            print(
                f"Error in line {line_num}: number is too large",
                file=sys.stderr,
            )
            continue

        key = (numpart, numidx)
        if key in msgs:
            print(
                f"Error in line {line_num}: duplicate number {num}",
                file=sys.stderr,
            )
        msgs[key] = True

        if numidx > msgidxmax[numpart]:
            msgidxmax[numpart] = numidx

        # Enum entry
        enum_parts.append((s[:i], number_str))

        # In the Pascal source (1-based indexing):
        #   j points to first non-digit after the number
        #   s[j+1] == '[' indicates multiline
        # In Python (0-based): j is the same offset, check s[j+1]
        if j + 1 < len(s) and s[j + 1] == "[":
            multiline = True
            current_multi_bytes = bytearray()
            # Prefix: everything from after '=' up to (and including)
            # the char before '['.  Original: Copy(s, i+1, j-i)
            prefix = s[i + 1 : j + 1].encode("latin-1")
            current_multi_bytes.extend(prefix)
        else:
            # Single line: everything after '=' + NUL
            txt = s[i + 1 :]
            msgtxt_parts.append(txt.encode("latin-1") + b"\x00")

    assert not multiline, f"Still in multiline mode at end of {filename}"

    # Concatenate parts; remove trailing NUL (original does dec(msgsize))
    msgtxt = b"".join(msgtxt_parts)
    if msgtxt and msgtxt[-1] == 0:
        msgtxt = msgtxt[:-1]

    return msgtxt, enum_parts, msgidxmax, codepage


def write_enum_file(filename, enum_entries, msgidxmax, msgsize):
    """Write the msgidx.inc file (WriteEnumFile in msg2inc.pp)."""
    with open(filename, "w", newline="\n") as f:
        f.write("const\n")
        for name, number in enum_entries:
            f.write(f"  {name}={number};\n")
        f.write("\n")
        f.write(f"  MsgTxtSize = {msgsize};\n")
        f.write("\n")
        f.write("  MsgIdxMax : array[1..20] of longint=(\n")
        f.write("    ")
        for i in range(1, 21):
            f.write(str(msgidxmax[i] + 1))
            if i < 20:
                f.write(",")
            if i == 10:
                f.write("\n    ")
        f.write("\n")
        f.write("  );\n")


def write_string_file(filename, constname, msgtxt, codepage):
    """
    Write the msgtxt.inc file in M_String format
    (WriteStringFile in msg2inc.pp).

    Produces a Pascal const array of string[240] / array of char.
    """
    msgsize = len(msgtxt)
    array_max = (msgsize - 1) // MAXSLEN if msgsize > 0 else 0

    with open(filename, "w", newline="\n") as f:
        f.write(f"const {constname}_codepage={codepage:5d};\n")
        f.write("{$ifdef Delphi}\n")
        f.write(
            f"const {constname} : array[0..{array_max:06d}]"
            f" of string[{MAXSLEN}]=(\n"
        )
        f.write("{$else Delphi}\n")
        f.write(
            f"const {constname} : array[0..{array_max:06d},"
            f"1..{MAXSLEN}] of char=("
        )
        f.write("{$endif Delphi}")

        slen = 0  # position within current MAXSLEN element
        line_len = 0  # current output line length
        in_quote = False
        is_start = True

        for byte_val in msgtxt:
            if slen >= MAXSLEN:
                if in_quote:
                    f.write("'")
                    in_quote = False
                f.write(",")
                slen = 0
                line_len += 1

            if line_len > 70 or is_start:
                if in_quote:
                    f.write("'")
                    in_quote = False
                if slen > 0:
                    f.write("+\n")
                else:
                    f.write("\n")
                line_len = 0
                is_start = False

            if line_len == 0:
                f.write("  ")
                line_len = 2

            if 32 <= byte_val <= 127 and byte_val != 39:
                if not in_quote:
                    f.write("'")
                    in_quote = True
                    line_len += 1
                f.write(chr(byte_val))
                line_len += 1
            else:
                if in_quote:
                    f.write("'")
                    line_len += 1
                    in_quote = False
                f.write("#" + str(byte_val).zfill(3))
                line_len += 4

            if byte_val in (0, 10):
                is_start = True

            slen += 1

        if in_quote:
            f.write("'")
        f.write("\n);\n")


def main():
    if len(sys.argv) != 4:
        print(
            f"Usage: {sys.argv[0]} <msgfile> <outbase> <constname>",
            file=sys.stderr,
        )
        sys.exit(1)

    msgfile = sys.argv[1]
    outbase = sys.argv[2]
    constname = sys.argv[3]

    msgtxt, enum_entries, msgidxmax, codepage = load_msg_file(msgfile)
    msgsize = len(msgtxt)

    enum_file = outbase + "idx.inc"
    txt_file = outbase + "txt.inc"

    write_enum_file(enum_file, enum_entries, msgidxmax, msgsize)
    write_string_file(txt_file, constname + "txt", msgtxt, codepage)


if __name__ == "__main__":
    main()
