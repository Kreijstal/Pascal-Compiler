Unit rax64int;

  interface

    uses
      aasmtai,
      rax86int;

    type
      tx8664intreader = class(tx86intreader)
        actsehdirective: TAsmSehDirective;
        function is_targetdirective(const s:string):boolean;override;
        procedure HandleTargetDirective;override;
      end;


  implementation

    uses
      globtype,
      cutils,
      systems,
      verbose,
      cgbase,
      symconst,
      procinfo,
      rabase;

    const
      { max offset and bitmask for .seh_savereg and .seh_setframe }
      maxoffset: array[boolean] of aint=(high(dword), 240);
      modulo: array[boolean] of integer=(7, 15);

    function tx8664intreader.is_targetdirective(const s:string):boolean;
      var
        i: TAsmSehDirective;
      begin
        result:=false;
        if target_info.system<>system_x86_64_win64 then exit;

        for i:=low(TAsmSehDirective) to high(TAsmSehDirective) do
          begin
            if not (i in recognized_directives) then
              continue;
            if s=sehdirectivestr[i] then
              begin
                actsehdirective:=i;
                result:=true;
                break;
              end;
          end;
      end;

end.
