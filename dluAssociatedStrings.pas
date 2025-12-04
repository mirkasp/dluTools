unit dluAssociatedStrings;

{$mode ObjFPC}{$H+}

interface

uses SysUtils, SyncObjs;
// TStringArray is assumed to be defined in SysUtils

// A list designed to store unique strings (case-insensitive)
// and maintain a separate 'DefaultValue'. It grows dynamically
// and automatically sorts and removes duplicates upon access (lazy evaluation).
type TAssociatedStrings = class
    strict private
        const DEFAULT_CAPACITY = 10; // Initial capacity increment when resizing
        var FValues           : TStringArray;     // Internal array storing non-default values
        var FCount            : integer;          // Current number of items in FValues
        var FDefaultValue     : string;           // The first unique value added (always lowercase)
        var FIsSorted         : boolean;          // Flag indicating if FValues is sorted and unique
        var FCapacity         : integer;          // Capacity increment size
        var FListLock         : TCriticalSection; // Lock for instance-specific thread safety

        function GetItem( const Index: integer) : string;
        function GetCount(): integer;

        // Sorts the internal array using Insertion Sort
        procedure PerformInsertionSort();
        // Sorts the array and removes duplicate entries, updates FCount.
        procedure EnsureSortedAndUnique();

    public
        constructor Create( const ACapacity: integer = DEFAULT_CAPACITY );
        destructor Destroy; override;

        // Adds a new string to the list (case-insensitive comparison/storage)
        procedure Add( const AValue: string );

        // Returns an array containing the DefaultValue at index 0, followed by
        // the sorted, unique list items. NOTE: This array is newly allocated.
        function GetValues: TStringArray;

        // The default value, stored separately and not part of the main list array
        property DefaultValue      : string  read FDefaultValue;
        // The number of unique, non-default strings in the list. Triggers sorting if needed.
        property Count             : integer read GetCount;
        // Accesses a unique string by index. Triggers sorting and duplicate removal.
        property Items[const Index: integer]: string  read GetItem; default;

end;

implementation

{ TAssociatedStrings }

constructor TAssociatedStrings.Create( const ACapacity: integer );
begin
    inherited Create;
    FValues         := nil;
    FCount          := 0;
    FDefaultValue   := '';
    FIsSorted       := false;
    // Create the critical section for this specific instance
    FListLock       := TCriticalSection.Create;

    // Ensure capacity is at least 1
    if ACapacity < 1 then FCapacity := DEFAULT_CAPACITY else FCapacity := ACapacity;
end;

destructor TAssociatedStrings.Destroy;
begin
    FListLock.Free; // Free the critical section
    inherited Destroy;
end;

procedure TAssociatedStrings.Add( const AValue: string);
    var S: string;
begin
    if AValue.IsEmpty then Exit;

    // Store and compare in lowercase to ensure case-insensitivity
    S := LowerCase(AValue);

    FListLock.Enter;
    try
        if FDefaultValue.IsEmpty then begin
            FDefaultValue := S;
        end else if S <> FDefaultValue then begin
            // Only add if it's not the default value
            // Resize array if capacity is reached
            if FCount = Length(FValues) then
                SetLength(FValues, FCount + FCapacity);

            FValues[FCount] := S;
            Inc(FCount);
            // Mark as unsorted to trigger update on next access
            FIsSorted := false;
        end;
    finally
        FListLock.Leave;
    end;
end ;

function TAssociatedStrings.GetValues: TStringArray;
    var i: integer;
begin
    // Ensure the list is sorted and unique before copying the data
    if not FIsSorted then EnsureSortedAndUnique();

    // Total size is FCount (items in FValues) + 1 (FDefaultValue)
    SetLength( Result, FCount + 1 );

    // 1. Copy the Default Value to the first element
    Result[0] := FDefaultValue;

    // 2. Copy the unique non-default items from FValues
    for i:=0 to FCount-1 do Result[ i+1 ] := FValues[ i ] ;
end;

// Implements Insertion Sort
procedure TAssociatedStrings.PerformInsertionSort();
    var I, J: Integer;
      Temp: String;
begin
    // Array with 0 or 1 element is already sorted
    if FCount > 1 then begin
        // Iterate from the second element to the end
        for I := 1 to FCount - 1 do begin
            Temp := FValues[I];
            J := I - 1;

            // Shift elements greater than 'Temp' to the right
            while (J >= 0) and (FValues[J] > Temp) do begin
                FValues[J + 1] := FValues[J];
                Dec(J);
            end;

            // Insert the element into the correct position
            FValues[J + 1] := Temp;
        end;
    end;
    FIsSorted := true;
end;

// Sorts the array and removes duplicate entries (requires a sorted array)
procedure TAssociatedStrings.EnsureSortedAndUnique( );
    var ReadIdx, WriteIdx: Integer;
begin
    FListLock.Enter;
    try
        // Safety check: ensure the array is sorted before removing duplicates
        if not FIsSorted then PerformInsertionSort();

        if FCount <= 1 then Exit;

        // WriteIdx points to the last unique element found
        WriteIdx := 0;

        // Iterate through the array starting from the second element
        for ReadIdx := 1 to FCount - 1 do begin
            // If the current element is different from the last unique one...
            if FValues[ReadIdx] <> FValues[WriteIdx] then begin
                Inc(WriteIdx);
                // ...copy it to the position immediately after the last unique one
                if WriteIdx <> ReadIdx
                   then FValues[WriteIdx] := FValues[ReadIdx];
            end;
        end;

        // The new element count is the index of the last written element + 1
        FCount := WriteIdx + 1;

    finally
        FListLock.Leave;
    end;
end;

function TAssociatedStrings.GetItem( const Index: integer) : string;
begin
    // Ensure the list is sorted and unique before accessing an item
    if not FIsSorted then EnsureSortedAndUnique();

    if (Index >= 0) and (Index < FCount)
        then Result := FValues[Index]
        else raise ERangeError.CreateFmt( 'Index out of range (%d). Valid range is 0 to %d.', [Index, FCount - 1] );
end;

function TAssociatedStrings.GetCount( ) : integer;
begin
    // Ensure the list is sorted and unique to return the correct unique count
    if not FIsSorted then EnsureSortedAndUnique();
    Result := FCount;
end ;

end.
