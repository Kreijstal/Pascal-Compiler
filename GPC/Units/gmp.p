unit gmp;

interface

uses
    SysUtils;

type
    MPInteger = NativeUInt;

procedure z_init(var value: MPInteger);
procedure z_clear(var value: MPInteger);
procedure z_set(var dest: MPInteger; const source: MPInteger);
procedure z_set_ui(var dest: MPInteger; new_value: NativeUInt);
procedure z_add_ui(var dest: MPInteger; addend: NativeUInt);
function z_add(const lhs, rhs: MPInteger): MPInteger;
procedure z_ui_pow_ui(var dest: MPInteger; base, exponent: NativeUInt);
function z_size(const value: MPInteger): NativeUInt;
function z_get_str(base: NativeUInt; const value: MPInteger): AnsiString;

implementation

procedure z_init(var value: MPInteger);
begin
    asm
        call gpc_mpz_init
    end;
end;

procedure z_clear(var value: MPInteger);
begin
    asm
        call gpc_mpz_clear
    end;
end;

procedure z_set(var dest: MPInteger; const source: MPInteger);
begin
    asm
        call gpc_mpz_set
    end;
end;

procedure z_set_ui(var dest: MPInteger; new_value: NativeUInt);
begin
    asm
        call gpc_mpz_set_ui
    end;
end;

procedure z_add_ui(var dest: MPInteger; addend: NativeUInt);
begin
    asm
        call gpc_mpz_add_ui
    end;
end;

function z_add(const lhs, rhs: MPInteger): MPInteger;
var
    result_handle: MPInteger;
begin
    asm
        call gpc_mpz_add
        movq %rax, -8(%rbp)
    end;
    z_add := result_handle;
end;

procedure z_ui_pow_ui(var dest: MPInteger; base, exponent: NativeUInt);
begin
    asm
        call gpc_mpz_ui_pow_ui
    end;
end;

function z_size(const value: MPInteger): NativeUInt;
var
    result_size: NativeUInt;
begin
    asm
        call gpc_mpz_size
        movq %rax, -8(%rbp)
    end;
    z_size := result_size;
end;

function z_get_str(base: NativeUInt; const value: MPInteger): AnsiString;
var
    result_ptr: AnsiString;
begin
    asm
        call gpc_mpz_get_str
        movq %rax, -8(%rbp)
    end;
    z_get_str := result_ptr;
end;

end.
