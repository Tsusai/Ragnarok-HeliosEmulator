(*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*

Unit
StaticDatabaseTemplate

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

Houses an abstract class, TStaticDatabaseTemplate, that defines all the calls
that are made to each specifically implemented database (SQLite, MySQL, MSSQL,
etc., etc.)

================================================================================
Revisions:
================================================================================
(Format: [yyyy/mm/dd] <Author> - <Desc of Changes>)
[2006/09/29] RaX - Created
[2007/04/07] CR - Altered header.  Made unit description accurate and focussed.
*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*)
unit StaticDatabaseTemplate;


interface


uses
	{RTL/VCL}
	Classes,
	{Project}
	Being,
	Character,
	MapTypes
	{Third Party}
	//none
	;


Type
(*= CLASS =====================================================================*
TStaticDatabaseTemplate

[2007/04/07] ChrstphrR

*------------------------------------------------------------------------------*
Overview:
*------------------------------------------------------------------------------*

	Abstract parent class for Static databases.  Defines the interface for all
	methods that the children will implement.

*------------------------------------------------------------------------------*
Revisions:
*------------------------------------------------------------------------------*
(Format: [yyyy/mm/dd] <Author> - <Description of Change>)
[2006/09/29] RaX - Created.
[2007/01/20] Tsusai - Connect is now a bool function
	Create holds connection result
[2007/04/07] CR - Altered header.  Made all methods abstract.  All parameters
	are strictly defined as in/out/var/const to self document and make string
	parameters more efficient when the methods are called.
*=============================================================================*)
TStaticDatabaseTemplate = class(TObject)
protected
	//none

public

	Function  GetBaseMaxHP(
		const
			ACharacter : TCharacter
		) : Word; virtual; abstract;

	Function  GetBaseMaxSP(
		const
			ACharacter : TCharacter
		) : Word; virtual; abstract;

	Function  GetBaseMaxWeight(
		const
			ACharacter : TCharacter
		) : LongWord; virtual; abstract;

	Function  GetMapCannotSave(
		const
			MapName : String
		) : Boolean; virtual; abstract;

	Function  GetMapZoneID(
		const
			MapName : String
		) : Integer; virtual; abstract;

	Function  GetMapFlags(
		const
			MapName : String
		) : TFlags; virtual; abstract;

	Function  GetMapsForZone(
		const
			ZoneID : LongWord
		) : TStringList; virtual; abstract;

	Function  GetBaseEXPToNextLevel(
		const
			JobName : String;
			Level : Word
		) : LongWord; virtual; abstract;

	Function  GetJobEXPToNextLevel(
		const
			JobName : String;
			Level : Word
		) : LongWord; virtual; abstract;

	Function GetStatPoints(
		const
			Level : Word
		) : LongWord; virtual; abstract;

	Function GetSkillPoints(
		const
			JobName : String;
			Level : Word
		) : LongWord; virtual; abstract;

	Function GetJobBonus(
		const
			JobName : String;
			Level : Word
		) : StatArray; virtual; abstract;

	function  Connect : Boolean; virtual; abstract;

	procedure Disconnect; virtual; abstract;

End;(* TStaticDatabaseTemplate
*== CLASS ====================================================================*)


implementation


end.
