//------------------------------------------------------------------------------
//InstanceMap                                                               UNIT
//------------------------------------------------------------------------------
//	What it does -
//		Object for instance map
//
//	Changes -
//		[2008/12/06] Aeomin - Created
//
//------------------------------------------------------------------------------
unit InstanceMap;

interface

uses
	Map;

type
	TInstanceMap = class(TMap)
	private
		//Instance Map ID
		Identifier : String;
	public
		BaseName : String;
		procedure Load(const InstanceIdentifier, OriginalMapName:String);reintroduce;
	end;
implementation


//------------------------------------------------------------------------------
//LOAD                                                                 PROCEDURE
//------------------------------------------------------------------------------
//	What it does -
//		Override of Load in TMap.
//
//	Changes -
//		[2008/12/06] Aeomin - Created.
//------------------------------------------------------------------------------
procedure TInstanceMap.Load(const InstanceIdentifier, OriginalMapName:String);
begin
	BaseName := OriginalMapName;
	Name := OriginalMapName;

	//Temporary hack..
	Path := 'Maps/'+Name+'.pms';

	Identifier := InstanceIdentifier;
	LoadFromFile(Path);
	inherited Load;
	Name := Identifier + '#'+ Name;
end;{Load}
//------------------------------------------------------------------------------


end{InstanceMap}.
