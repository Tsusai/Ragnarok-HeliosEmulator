(*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*

Unit
GameDatabaseTemplate

*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*

[2006/09/29] Helios - RaX

================================================================================
License:  (FreeBSD, plus commercial with written permission clause.)
================================================================================

Project Helios - Copyright (c) 2005-2007

All rights reserved.

Please refer to Helios.dpr for full license terms.

================================================================================
Overview:
================================================================================

	This is the parent class of our other database objects, it also includes a
function for choosing which database we're using based on a configuration
variable. It contains all database interface routines.

================================================================================
Revisions:
================================================================================
(Format: [yyyy/mm/dd] <Author> - <Desc of Changes>)
[2006/09/29] RaX - Created.
[2007/04/06] CR - Altered header, improved description.  Template class made
	entirely abstract, and all parameters are specified as in/out/var/const 
	explicitly.
*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*)


unit GameDatabaseTemplate;


interface


uses
	{RTL/VCL}
	//none
	{Project}
	Character,
	CharaList
	{Third Party}
	//none
	;


type


(*= CLASS =====================================================================*
TGameDatabaseTemplate

[2006/09/29] RaX

*------------------------------------------------------------------------------*
Overview:
*------------------------------------------------------------------------------*

	This is the abstract parent class for the Game database.
Defines all database routines common to the child classes that implement
specific databases (i.e. SQLite, MySQL, etc.).

*------------------------------------------------------------------------------*
Revisions:
*------------------------------------------------------------------------------*
(Format: [yyyy/mm/dd] <Author> - <Description of Change>)
[2006/09/29] RaX - Created unit
[2007/01/20] Tsusai - Connect is now a bool function
	Create holds connection result
[2007/04/06] CR - Altered header.  All methods made abstract, and all parameters
	now specified by in/out/var/const to self-document.
[2007/06/30] Tsusai - Added Get/SetCharaVariable
*=============================================================================*)
TGameDatabaseTemplate = class(TObject)
protected
	//none

public

	function  CharaExists(
		const
			AccountID : LongWord;
		const
			Slot : Word
		) : Boolean; overload; virtual; abstract;

	function  CharaExists(
		const
			Name : String
		) : Boolean; overload; virtual; abstract;

	function  CreateChara(
		var
			ACharacter : TCharacter;
		const
			AID        : LongWord;
		const
			NName      : String;
		const
			CharaNum   : Integer
		) : Boolean; virtual; abstract;

	function  DeleteChara(
		var
			ACharacter : TCharacter
		) : Boolean; virtual; abstract;

	function  GetAccountCharas(
		const
			AccountID : LongWord
		) : TCharacterList; virtual; abstract;

	function  GetChara(
		const
			CharaID : LongWord
		) : TCharacter; overload; virtual; abstract;

	function  GetChara(
		const
			CharaName : String
		) : TCharacter; overload; virtual; abstract;

	function  LoadChara(
		const
			CharaID : LongWord
		) : TCharacter; overload; virtual; abstract;

	function  LoadChara(
		const
			CharaName : String
		) : TCharacter; overload; virtual; abstract;

	procedure SaveChara(
		const
			AChara : TCharacter
		); virtual; abstract;
	
	function GetCharaVariable(
		const 
			AChara : TCharacter; 
		const 
			Key : string
		) : integer; virtual; abstract;

	procedure SetCharaVariable(
		const 
			AChara : TCharacter; 
		const 
			Key : string;
		const 
			Value : integer
		); virtual; abstract;

	function  Connect : Boolean; virtual; abstract;

	procedure Disconnect; virtual; abstract;

End;(* TGameDatabaseTemplate
*== CLASS ====================================================================*)


implementation


end.
