(*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*

Unit
CommonDatabaseTemplate

*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*

[2007/04/06] Helios - RaX

================================================================================
License:  (FreeBSD, plus commercial with written permission clause.)
================================================================================

Project Helios - Copyright (c) 2005-2007

All rights reserved.

Please refer to Helios.dpr for full license terms.

================================================================================
Overview:
================================================================================

	This is the parent class of the "Common" database objects, those used to store
and manipulate Player Accounts.  These databases are primarily accessed by the
Login server.

	The abstract class defined here also includes a function for choosing which 
database we're using based on a configuration variable.  Since this is an
abstract class, it defines the interface for descendant classes to follow, for
their specific Database (i.e. SQLITE, MySQL, MSSQL, etc., etc.)

================================================================================
Revisions:
================================================================================
(Format: [yyyy/mm/dd] <Author> - <Desc of Changes>)
[2006/09/29] RaX - Created unit
[2007/04/06] CR - Changed Header, included license.  Updated description for
	clarity's sake.  Made class entirely abstract, and made all routines define
	each parameter as either in/out/var/const to self-document.
*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*)

unit CommonDatabaseTemplate;


interface


uses
	{RTL/VCL}
	//none
	{Project}
	Account
	{Third Party}
	//none
	;


type


(*= CLASS =====================================================================*
TCommonDatabaseTemplate

[2006/09/29] RaX

*------------------------------------------------------------------------------*
Overview:
*------------------------------------------------------------------------------*

	This is an abstract parent class for Account databases.
	Defines all database routines for the Common Database used primarily by the
	Login Server (for Account information).

*------------------------------------------------------------------------------*
Revisions:
*------------------------------------------------------------------------------*
(Format: [yyyy/mm/dd] <Author> - <Description of Change>)
[2006/09/29] RaX - Created.
[2007/01/20] Tsusai - Connect is now a bool function
[2007/04/06] CR - Changed Header Style.  Made the class entirely Abstract.
	All methods state explicit in/out/var/const for each parameter.
	Eliminated Constructor and Destructor definitions because they are identical
	to TObject's -- and all derived classes have more complex constructors.
*=============================================================================*)
TCommonDatabaseTemplate = class(TObject)
protected
	//none
public

	function  AccountExists(
		const
			UserName : String
		) : Boolean; virtual; abstract;

	procedure CreateAccount(
		const
			Username : String;
		const
			Password : String;
		const
			GenderChar : Char
		); virtual; abstract;

	function  Connect : Boolean; virtual; abstract;

	procedure Disconnect; virtual; abstract;

	function  GetAccount(
		const
			ID    : LongWord
		) : TAccount; overload; virtual; abstract;

	function  GetAccount(
		const
			Name  : String
		) : TAccount; overload; virtual; abstract;

	procedure RefreshAccountData(
		var
			AnAccount : TAccount
		); virtual; abstract;

	procedure SaveAccount(
		const
			AnAccount : TAccount
		); virtual; abstract;

End;(* TCommonDatabaseTemplate
*== CLASS ====================================================================*)


implementation


end.
