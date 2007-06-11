unit OnTouchCellEvent;

interface
uses
	NPC,
	Character;

type
	TOnTouchCellEvent = class
		protected
			fScriptNPC : TScriptNPC;
		public
			property ScriptNPC : TScriptNPC read fScriptNPC;
			Constructor Create(AScriptNPC : TScriptNPC);
			Procedure Execute(ACharacter : TCharacter);
	end;
implementation

	Constructor TOnTouchCellEvent.Create(AScriptNPC : TScriptNPC);
	begin
		inherited Create;
		fScriptNPC := AScriptNPC;
	end;

	//[2007/05/28] Tsusai - Added execution checks.
	Procedure TOnTouchCellEvent.Execute(ACharacter : TCharacter);
	begin
		if (ACharacter.ScriptStatus = SCRIPT_NOTRUNNING) and
		(ACharacter.CharaState = charaStanding) then
		begin
			ACharacter.ScriptID := fScriptNPC.ID;
			fScriptNPC.OnTouch(ACharacter);
		end;
	end;
end.