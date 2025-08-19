program sign(input, output, stderr);

type
	signumCodomain = -1..1;

var
	x: longint;

{ returns the sign of an integer }
function signum({$ifNDef CPUx86_64} const {$endIf} x: longint): signumCodomain;
{$ifDef CPUx86_64_DISABLED} // ============= optimized implementation
assembler;
asm
	xorq %rax, %rax
	testq %rdi, %rdi
	setnz %al
	sarq $63, %rdi
	cmovs %rdi, %rax
end
{$else} // ========================== default implementation
begin
	// This is what math.sign virtually does.
	// The compiled code requires _two_ cmp instructions, though.
	if x > 0 then
	begin
		signum := 1;
	end
	else if x < 0 then
	begin
		signum := -1;
	end
	else
	begin
		signum := 0;
	end;
end;
{$endIf}

// M A I N =================================================
begin
	read(x);
	writeln(signum(x));
end.
