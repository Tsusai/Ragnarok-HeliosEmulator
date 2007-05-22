unit OnTouchCellEvent;

interface
uses
	NPC,
	Character;

type
	TOnTouchCellEvent = class(TObject)
		protected


		public
			ScriptNPC : TScriptNPC;

			Constructor Create(AScriptNPC : TScriptNPC);
			Procedure Execute(ACharacter : TCharacter);
	end;
implementation

	Constructor TOnTouchCellEvent.Create(AScriptNPC : TScriptNPC);
	begin
		inherited Create;
		ScriptNPC := AScriptNPC;
	end;

	Procedure TOnTouchCellEvent.Execute(ACharacter : TCharacter);
	begin
		ScriptNPC.OnTouch(ACharacter);
  end;
end.
