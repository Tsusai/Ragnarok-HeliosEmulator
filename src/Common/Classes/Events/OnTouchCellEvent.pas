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

	Procedure TOnTouchCellEvent.Execute(ACharacter : TCharacter);
	begin
		fScriptNPC.OnTouch(ACharacter);
	end;
end.
