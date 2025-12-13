{ Test const array bounds with const expression - required for FPC bootstrap }
{ errors.pp uses sys_errn-1 as upper bound }
program fpc_bootstrap_const_array_expr_bounds;

{$mode objfpc}

const
  NUM_ITEMS = 3;
  Items: array[0..NUM_ITEMS-1] of PAnsiChar = (
    'First',
    'Second',
    'Third'
  );

begin
  WriteLn('Items[0] = ', Items[0]);
  WriteLn('Items[1] = ', Items[1]);
  WriteLn('Items[2] = ', Items[2]);
end.
