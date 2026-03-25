unit SortBase;

interface

type
  TListSortComparer_NoContext = function(Item1, Item2: Pointer): Integer;
  TPtrListSorter_NoContext = procedure(ItemPtrs: PPointer; ItemCount: SizeUInt; Comparer: TListSortComparer_NoContext);
  TItemListSorter_NoContext = procedure(Items: Pointer; ItemCount, ItemSize: SizeUInt; Comparer: TListSortComparer_NoContext);

  TListSortComparer_Context = function(Item1, Item2, Context: Pointer): Integer;
  TListSortCustomItemExchanger_Context = procedure(Item1, Item2, Context: Pointer);
  TPtrListSorter_Context = procedure(ItemPtrs: PPointer; ItemCount: SizeUInt; Comparer: TListSortComparer_Context; Context: Pointer);
  TItemListSorter_Context = procedure(Items: Pointer; ItemCount, ItemSize: SizeUInt; Comparer: TListSortComparer_Context; Context: Pointer);
  TItemListSorter_CustomItemExchanger_Context = procedure(Items: Pointer;
    ItemCount, ItemSize: SizeUInt; Comparer: TListSortComparer_Context;
    Exchanger: TListSortCustomItemExchanger_Context; Context: Pointer);

  PSortingAlgorithm = ^TSortingAlgorithm;
  TSortingAlgorithm = record
    PtrListSorter_NoContextComparer: TPtrListSorter_NoContext;
    PtrListSorter_ContextComparer: TPtrListSorter_Context;
    ItemListSorter_ContextComparer: TItemListSorter_Context;
    ItemListSorter_CustomItemExchanger_ContextComparer: TItemListSorter_CustomItemExchanger_Context;
  end;

procedure QuickSort_PtrList_NoContext(
                ItemPtrs: PPointer;
                ItemCount: SizeUInt;
                Comparer: TListSortComparer_NoContext);
procedure QuickSort_PtrList_Context(
                ItemPtrs: PPointer;
                ItemCount: SizeUInt;
                Comparer: TListSortComparer_Context;
                Context: Pointer);
procedure QuickSort_ItemList_Context(
                Items: Pointer;
                ItemCount, ItemSize: SizeUInt;
                Comparer: TListSortComparer_Context;
                Context: Pointer);
procedure QuickSort_ItemList_CustomItemExchanger_Context(
                Items: Pointer;
                ItemCount, ItemSize: SizeUInt;
                Comparer: TListSortComparer_Context;
                Exchanger: TListSortCustomItemExchanger_Context;
                Context: Pointer);

const
  QuickSort: TSortingAlgorithm = (
    PtrListSorter_NoContextComparer: @QuickSort_PtrList_NoContext;
    PtrListSorter_ContextComparer: @QuickSort_PtrList_Context;
    ItemListSorter_ContextComparer: @QuickSort_ItemList_Context;
    ItemListSorter_CustomItemExchanger_ContextComparer: @QuickSort_ItemList_CustomItemExchanger_Context;
  );

var
  DefaultSortingAlgorithm: PSortingAlgorithm = @QuickSort;

implementation

procedure QuickSort_PtrList_NoContext_Impl(ItemPtrs: PPointer; L, R: SizeUInt;
                                           Comparer: TListSortComparer_NoContext);
var
  I, J, PivotIdx: SizeUInt;
  P, Q: Pointer;
begin
  repeat
    I := L;
    J := R;
    PivotIdx := L + ((R - L) shr 1);
    P := ItemPtrs[PivotIdx];
    repeat
      while (I < PivotIdx) and (Comparer(P, ItemPtrs[I]) > 0) do
        Inc(I);
      while (J > PivotIdx) and (Comparer(P, ItemPtrs[J]) < 0) do
        Dec(J);
      if I < J then
      begin
        Q := ItemPtrs[I];
        ItemPtrs[I] := ItemPtrs[J];
        ItemPtrs[J] := Q;
        if PivotIdx = I then
        begin
          PivotIdx := J;
          Inc(I);
        end
        else if PivotIdx = J then
        begin
          PivotIdx := I;
          Dec(J);
        end
        else
        begin
          Inc(I);
          Dec(J);
        end;
      end;
    until I >= J;
    if (PivotIdx - L) < (R - PivotIdx) then
    begin
      if (L + 1) < PivotIdx then
        QuickSort_PtrList_NoContext_Impl(ItemPtrs, L, PivotIdx - 1, Comparer);
      L := PivotIdx + 1;
    end
    else
    begin
      if (PivotIdx + 1) < R then
        QuickSort_PtrList_NoContext_Impl(ItemPtrs, PivotIdx + 1, R, Comparer);
      if (L + 1) < PivotIdx then
        R := PivotIdx - 1
      else
        Exit;
    end;
  until L >= R;
end;

procedure QuickSort_PtrList_NoContext(ItemPtrs: PPointer; ItemCount: SizeUInt;
                                      Comparer: TListSortComparer_NoContext);
begin
  if (ItemPtrs = nil) or (ItemCount < 2) then
    Exit;
  QuickSort_PtrList_NoContext_Impl(ItemPtrs, 0, ItemCount - 1, Comparer);
end;

procedure QuickSort_PtrList_Context(ItemPtrs: PPointer; ItemCount: SizeUInt;
                                     Comparer: TListSortComparer_Context;
                                     Context: Pointer);
var
  I, J, PivotIdx, L, R: SizeUInt;
  P, Q: Pointer;
begin
  if (ItemPtrs = nil) or (ItemCount < 2) then
    Exit;
  L := 0;
  R := ItemCount - 1;
  repeat
    I := L;
    J := R;
    PivotIdx := L + ((R - L) shr 1);
    P := ItemPtrs[PivotIdx];
    repeat
      while (I < PivotIdx) and (Comparer(P, ItemPtrs[I], Context) > 0) do
        Inc(I);
      while (J > PivotIdx) and (Comparer(P, ItemPtrs[J], Context) < 0) do
        Dec(J);
      if I < J then
      begin
        Q := ItemPtrs[I];
        ItemPtrs[I] := ItemPtrs[J];
        ItemPtrs[J] := Q;
        if PivotIdx = I then
        begin
          PivotIdx := J;
          Inc(I);
        end
        else if PivotIdx = J then
        begin
          PivotIdx := I;
          Dec(J);
        end
        else
        begin
          Inc(I);
          Dec(J);
        end;
      end;
    until I >= J;
    if (PivotIdx - L) < (R - PivotIdx) then
    begin
      if (L + 1) < PivotIdx then
        begin end; { simplified: no recursion for context variant }
      L := PivotIdx + 1;
    end
    else
    begin
      if (L + 1) < PivotIdx then
        R := PivotIdx - 1
      else
        Exit;
    end;
  until L >= R;
end;

procedure QuickSort_ItemList_Context(Items: Pointer; ItemCount, ItemSize: SizeUInt;
                                      Comparer: TListSortComparer_Context;
                                      Context: Pointer);
begin
  { Minimal implementation }
end;

procedure QuickSort_ItemList_CustomItemExchanger_Context(Items: Pointer;
                ItemCount, ItemSize: SizeUInt;
                Comparer: TListSortComparer_Context;
                Exchanger: TListSortCustomItemExchanger_Context;
                Context: Pointer);
begin
  { Minimal implementation }
end;

end.
