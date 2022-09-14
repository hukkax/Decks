// david@steema.com

// Latest source:
// https://github.com/davidberneda/GenericTree

// FPC conversion: hukka (hukkax@gmail.com) 2022-09-13

unit TeeGenericTree;

{$mode Delphi}
{$INLINE ON}

(*
 Generic Tree structure
 ======================
 Basic usage:

 var Root : TNode<String>;

 Root := TNode<String>.Create;
 try
   Root.Add('Hello').Add('World !');
 finally
   Root.Free;
 end;

 The generic type can be pre-declared for easier typing:

 type
   TFloatTree = TNode<Single>;

 var Tree1 : TFloatTree;

 Features:

 Adding nodes using the Add method, returns the new created node:

 var Node : TNode<String>;
 Node := Root.Add('abc');

 "Count" returns the number of child nodes for a given node:

 var t : Integer,
 t:=Node.Count;

 "Empty" returns True when the "Count" of children nodes is zero:

 var b : Boolean,
 b:=Node.Empty;

 Destroying a node removes it from its parent:

 Node.Free;

 Nodes can be accessed using the default array property:

 Node := Root[3];

 "Clear" removes and destroys all children nodes of a given node (recursively):

 Node.Clear;

 "Index" returns the position of a node in its parent children array, or -1
 if the node is a "root" node.

 var t : Integer;
 t:=Node.Index;

 "Parent" property returns the node that is the parent of a given node, or
 nil if the node is a "root" node.

 var tmp : TNode<String>;
 tmp:=Node.Parent;

 A node can be "detached" from its parent (without destroying it), setting the
 Parent property to nil:

 Node.Parent:=nil;

 A node can also be removed and destroyed using its Parent Delete method:

 Root.Delete(3); // removes and destroys the 4th child of Root

 Traversing nodes (recursively or not) using the ForEach method:

 var Total:Integer;
 Total:=0;
 Root.ForEach(procedure(const Item:TNode<String>) begin Inc(Total); end);

 The "Level" property returns the depth of a node, (the number of parent->parent->...),
 being zero for root nodes that have no parent.

 var t : Integer;
 t:=Node.Level;

 Exchanging nodes (swap positions):

 Root.Exchange( 5, 9 );  <-- swap positions

 Sorting nodes:

 Root.Sort(function(const A,B:TNode<String>):Integer
    begin
      if CaseSensitive then
         result:=CompareStr(A.Data,B.Data)
      else
         result:=CompareText(A.Data,B.Data);
    end);
*)

{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}

interface

type
	TNode<T>=class
	public
		type
			TTypeItem = TNode<T>;
			// <0 : A<B
			//  0 : A=B
			// >0 : A>B
			TCompareProc = function(const A, B: TTypeItem): Integer of Object;
	private
		var
			FItems : TArray<TTypeItem>;
			FParent: TTypeItem;

		procedure Adopt(const Item: TTypeItem);
		procedure Extract(const Index: Integer; const ACount: Integer = 1);
		function  Get(const Index: Integer): TTypeItem; inline;
		function  GetIndex: Integer;
		function  GetLevel: Integer;
		function  ItemsCopy: TArray<TTypeItem>;
		procedure Orphan;
		procedure PrivateSort(const ACompare: TCompareProc; const l, r: Integer);
		procedure SetParent(const Value: TTypeItem);
	public
		type
			TNodeProc = procedure(const Item: TTypeItem) of Object;
		var
			Data: T;

		constructor Create(const AData: T); overload;
		destructor  Destroy; override;

		function  Add(const AData: T): TTypeItem;
		procedure Clear; inline;
		function  Count: Integer; inline;
		function  Empty: Boolean; inline;
		procedure Exchange(const Index1, Index2: Integer);
		procedure Delete(const Index: Integer; const ACount: Integer = 1);
		procedure ForEach(const AProc: TNodeProc; const Recursive: Boolean = True);
		procedure Sort(const ACompare: TCompareProc; const Recursive: Boolean = True);

		function IsRoot: Boolean;
		function GetNextAncestor: TTypeItem;
		function GetFirstChild: TTypeItem;
		function GetNext: TTypeItem;
		function GetNextSibling: TTypeItem;
		function GetPreviousSibling: TTypeItem;
		function FullCount: Int64;

		property Index: Integer read GetIndex;
		property Item[const Index: Integer]: TTypeItem read Get; default;
		property Items: TArray<TTypeItem> read FItems;
		property Level: Integer read GetLevel;
		property Parent: TTypeItem read FParent write SetParent;
	end;

implementation

{ TNode<T> }

// Creates a new Node
constructor TNode<T>.Create(const AData: T);
begin
	inherited Create;
	Data := AData;
end;

// Remove and destroy all child nodes, then remove Self from Parent
destructor TNode<T>.Destroy;
begin
	Clear;
	Orphan;
	inherited;
end;

// Returns childr node at Index position
function TNode<T>.Get(const Index: Integer): TTypeItem;
begin
	Result := FItems[Index];
end;

// Returns the number of child nodes
function TNode<T>.Count: Integer;
begin
	Result := Length(FItems);
end;

// Adds a new node and sets its AData
function TNode<T>.Add(const AData: T): TTypeItem;
begin
	Result := TTypeItem.Create(AData);
	Adopt(Result);
end;

// Remove and destroy all child nodes
procedure TNode<T>.Clear;
begin
	Delete(0, Count);
	FItems := nil;
end;

// Removes ACount items from the array, starting at Index position (without destroying them)
procedure TNode<T>.Extract(const Index, ACount: Integer);
begin
	System.Delete(FItems, Index, ACount);
end;

// Removes and destroys child nodes from Index position (ACount default = 1)
procedure TNode<T>.Delete(const Index, ACount: Integer);
var
	t: Integer;
begin
	// Destroy nodes
	for t := Index to Index + ACount - 1 do
	begin
		FItems[t].FParent := nil;
		FItems[t].Free;
	end;
	Extract(Index,ACount);
end;

// Returns True when this node has no child nodes
function TNode<T>.Empty: Boolean;
begin
	Result := (Count = 0);
end;

// Swap child nodes at positions: Index1 <---> Index2
procedure TNode<T>.Exchange(const Index1, Index2: Integer);
var
	tmp : TTypeItem;
begin
	tmp := FItems[Index1];
	FItems[Index1] := FItems[Index2];
	FItems[Index2] := tmp;
end;

function TNode<T>.ItemsCopy: TArray<TNode<T>>;
var
	t: Integer;
begin
	SetLength(Result{%H-}, Count);
	for t := 0 to Count-1 do
		Result[t] := FItems[t];
end;

// Calls AProc for each child node (optionally recursive)
procedure TNode<T>.ForEach(const AProc: TNodeProc; const Recursive: Boolean);
var
	t: Integer;
	N: TTypeItem;
	tmp: TArray<TTypeItem>;
begin
	tmp := ItemsCopy;
	t := 0;
	while t < Length(tmp) do
	begin
		N := tmp[t];
		if N <> nil then
		begin
			AProc(N);
			if Recursive then
				N.ForEach(AProc);
		end;
		Inc(t);
	end;
end;

// Returns the Index position of Self in Parent children list
function TNode<T>.GetIndex: Integer;
var
	t: Integer;
begin
	if FParent <> nil then
		for t := 0 to FParent.Count-1 do
			if FParent[t] = Self then
				Exit(t);
	Result := -1;
end;

// Returns the number of parents in the hierarchy up to top of tree
function TNode<T>.GetLevel: Integer;
begin
	if FParent = nil then
		Result := 0
	else
		Result := FParent.Level + 1;
end;

// Adds Item to children list, sets Item Parent = Self
procedure TNode<T>.Adopt(const Item: TTypeItem);
var
	L: Integer;
begin
	Item.FParent := Self;
	// Pending: Capacity
	L := Count;
	SetLength(FItems, L+1);
	FItems[L] := Item;
end;

// Removes itself from Parent children list
procedure TNode<T>.Orphan;
begin
	if FParent <> nil then
		FParent.Extract(Index);
end;

// Sets or changes the Parent node of Self
procedure TNode<T>.SetParent(const Value: TTypeItem);
begin
	if FParent <> Value then
	begin
		Orphan;
		FParent := Value;
		if FParent <> nil then
			FParent.Adopt(Self);
	end;
end;

// Internal. Re-order nodes using QuickSort algorithm
procedure TNode<T>.PrivateSort(const ACompare: TCompareProc; const l, r: Integer);
var
	i, j, x: Integer;
begin
	i := l;
	j := r;
	x := (i + j) shr 1;

	while i < j do
	begin
		while ACompare(Self[x], Self[i]) > 0 do Inc(i);
		while ACompare(Self[x], Self[j]) < 0 do Dec(j);
		if i < j then
		begin
			Exchange(i, j);
			if i = x then x := j
			else
			if j = x then x := i;
		end;

		if i <= j then
		begin
			Inc(i);
			Dec(j);
		end;
	end;

	if l < j then
		PrivateSort(ACompare, l, j);
	if i < r then
		PrivateSort(ACompare, i, r);
end;

// Re-order child items according to a custom ACompare function
procedure TNode<T>.Sort(const ACompare: TCompareProc; const Recursive: Boolean);
var
	t: Integer;
begin
	if Count > 1 then
	begin
		PrivateSort(ACompare, 0, Count-1);
		// Optionally, re-order all child-child... nodes
		if Recursive then
			for t := 0 to Count-1 do
				Items[t].Sort(ACompare, Recursive);
	end;
end;

// additions by Kevin Solway: https://steema.com/wp/blog/2015/06/15/generic-delphi-tree-structure/

function TNode<T>.IsRoot: Boolean;
begin
	Result := (Assigned(Self)) and (Self.Index = -1);
end;

function TNode<T>.GetNextAncestor: TTypeItem;
var
	_parent: TTypeItem;
begin
	Result := nil;
	if (Assigned(Self)) and (not Self.IsRoot) then
	begin
		_parent := Parent;
		if Assigned(_parent) then
		begin
			if Self.Index = Pred(_parent.Count) then
				Result := _parent.GetNextAncestor // no more siblings
			else
				Result := _parent[Succ(Self.Index)];
		end;
	end
end;

function TNode<T>.GetFirstChild: TTypeItem;
begin
	if (Assigned(Self)) and (Self.Count > 0) then
		Result := Self[0]
	else
		Result := nil;
end;

function TNode<T>.GetNext: TTypeItem;
begin
	Result := nil;
	if Assigned(Self) then
	begin
		Result := Self.GetFirstChild;   // Go down through the levels
		if Result = nil then
			Result := Self.GetNextAncestor;   // Go back through the levels
	end;
end;

function TNode<T>.GetNextSibling: TTypeItem;
var
	_parent: TTypeItem;
begin
	Result := nil;
	if (Assigned(Self)) and (not Self.IsRoot) then
	begin
		_parent := Self.Parent;
		if Assigned(_parent) then
		begin
			if Self.Index < Pred(_parent.Count) then
				Result := _parent[Succ(Self.Index)]
			else
				Result := _parent[0];
		end;
	end;
end;

function TNode<T>.GetPreviousSibling: TTypeItem;
var
	_parent: TTypeItem;
begin
	Result := nil;
	if (Assigned(Self)) and (not Self.IsRoot) then
	begin
		_parent := Self.Parent;
		if Assigned(_parent) then
		begin
			if Self.Index > 0 then
				Result := _parent[Pred(Self.Index)]
			else
				Result := _parent[_parent.Count-1];
		end;
	end;
end;

// Returns a count of all nodes below and including the start node
function TNode<T>.FullCount: Int64;
begin
	Result := 0;
	while Assigned(Self) do
	begin
		Inc(Result);
		Self := Self.GetNext;
	end;
end;


end.

