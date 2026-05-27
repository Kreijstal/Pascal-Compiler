unit tdd_pp_bootstrap_longbool_boolean_assign_helper;

{ Mirrors FPC's wininc/unifun.inc which declares MoveFileW as returning
  WINBOOL (= LongBool).  This unit's public interface owns the
  preferred declaration of TwinSym. }

interface

type
   WINBOOL = LongBool;

function TwinSym(x: LongInt): WINBOOL;

implementation

function TwinSym(x: LongInt): WINBOOL;
begin
    TwinSym := x <> 0;
end;

end.
