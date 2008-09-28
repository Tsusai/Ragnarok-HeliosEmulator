//------------------------------------------------------------------------------
//GameTypes                                                                UNIT
//------------------------------------------------------------------------------
//	What it does-
//			Contains game related Types
//
//	Changes -
//		February 25th, 2008 - RaX - Created
//
//------------------------------------------------------------------------------

unit GameTypes;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}


interface


//uses
	{RTL/VCL}
	{Project}
	{Third Party}

type
	TLookTypes =
	(
		LOOK_JOB,
		LOOK_HAIR,
		LOOK_WEAPON,
		LOOK_HEAD_BOTTOM,
		LOOK_HEAD_TOP,
		LOOK_HEAD_MID,
		LOOK_HAIR_COLOR,
		LOOK_CLOTHES_COLOR,
		LOOK_SHIELD,
		LOOK_SHOES,
		LOOK_MOB
	);
	
	TGenders =
	(
		MALE,
		FEMALE,
		BOTH
	);

function GenderToChar(const AGender : TGenders) : Char;
function CharToGender(AChar : Char) : TGenders;


implementation

function GenderToChar(const AGender : TGenders) : Char;
begin
	case AGender of
		MALE :
			begin
				Result := 'm';
			end;
		FEMALE		:
			begin
				Result := 'f';
			end;
		else
		begin
			Result := 's';
		end;
	end;
end;


function CharToGender(AChar : Char) : TGenders;
begin
	case AChar of
		'm',
		'M':
			begin
				Result := MALE;
			end;
		'f',
		'F':
			begin
				Result := FEMALE;
			end;
		else
		begin
			Result := BOTH;
		end;
	end;
end;

end.
