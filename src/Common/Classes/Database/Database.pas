//------------------------------------------------------------------------------
//Database			                                                           UNIT
//------------------------------------------------------------------------------
//	What it does-
//			This is the base database class, which all other database implementations
//		are derived.
//
//	Changes -
//		February 1st, 2008
//
//------------------------------------------------------------------------------
unit Database;

{$IFDEF FPC}
	{$MODE Delphi}
{$ENDIF}


interface


uses
	{RTL/VCL}
	{Project}
	DatabaseOptions,
	AccountQueries,
	CharacterQueries,
	FriendQueries,
	MapQueries,
	MailQueries,
	ItemQueries,
	CharacterConstantQueries,
	{3rd Party}
	IdThread,
	ZConnection,
	ZSqlUpdate
	;


type

//------------------------------------------------------------------------------
//TDatabase			                                                          CLASS
//------------------------------------------------------------------------------
	TDatabase = class(TObject)

	public
		AccountConnection			: TZConnection;
		GameConnection				: TZConnection;

		Options : TDatabaseOptions;

		Account			: TAccountQueries;
		Character		: TCharacterQueries;
		Friend			: TFriendQueries;
		Map					: TMapQueries;
		Mail			: TMailQueries;
		Items			: TItemQueries;
		CharacterConstant: TCharacterConstantQueries;

		Constructor Create();

		Destructor  Destroy
		(

		);override;


		Procedure Connect(

		);

		Procedure Disconnect(

		);

		Procedure LoadOptions(

		);
	end;
//------------------------------------------------------------------------------


implementation


uses
	{RTL/VCL}
	SysUtils,
	{Project}
	Main,
	Globals
	{3rd Party}
	//none
	;


//------------------------------------------------------------------------------
//Create			                                                     CONSTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Builds our object.
//
//	Changes -
//		February 1st, 2008 - RaX - Created
//
//------------------------------------------------------------------------------
Constructor TDatabase.Create();
Begin
	Inherited Create;
	LoadOptions;
	AccountConnection						:= TZConnection.Create(nil);
	AccountConnection.Protocol	:= Options.AccountConfig.Protocol;
	AccountConnection.HostName	:= Options.AccountConfig.Host;
	AccountConnection.Port			:= Options.AccountConfig.Port;
	AccountConnection.Database	:= Options.AccountConfig.Database;
	AccountConnection.User			:= Options.AccountConfig.User;
	AccountConnection.Password	:= Options.AccountConfig.Pass;

	GameConnection							:= TZConnection.Create(nil);
	GameConnection.Protocol			:= Options.GameConfig.Protocol;
	GameConnection.HostName			:= Options.GameConfig.Host;
	GameConnection.Port					:= Options.GameConfig.Port;
	GameConnection.Database			:= Options.GameConfig.Database;
	GameConnection.User					:= Options.GameConfig.User;
	GameConnection.Password			:= Options.GameConfig.Pass;

	Account											:= TAccountQueries.Create(AccountConnection);
	Character										:= TCharacterQueries.Create(GameConnection);
	Friend											:= TFriendQueries.Create(GameConnection);
	Map													:= TMapQueries.Create(GameConnection);
	CharacterConstant						:= TCharacterConstantQueries.Create(GameConnection);
	Mail								:= TMailQueries.Create(GameConnection);
	Items					:= TItemQueries.Create(GameConnection);
End;{Create}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//Destroy							                                               DESTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Frees up our custom properties.
//
//	Changes -
//		February 1st, 2008 - RaX - Created.
//
//------------------------------------------------------------------------------
Destructor TDatabase.Destroy(

);
begin

	Disconnect();

	Account.Free;
	Character.Free;
	Friend.Free;
	Map.Free;
	CharacterConstant.Free;
	Mail.Free;
	Items.Free;
	
	AccountConnection.Free;
	GameConnection.Free;

	Options.Save;
	Options.Free;

	Inherited;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Connect							                                               PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Connects to the database.
//
//	Changes -
//		February 1st, 2008 - RaX - Created.
//
//------------------------------------------------------------------------------
Procedure TDatabase.Connect(

);
begin
	try

		if NOT AccountConnection.Connected then
		begin
			AccountConnection.Connect();
		end;

	except
		on E : Exception do
		begin
			Console.Message('Could not connect to '+AccountConnection.Protocol+
				' database "'+AccountConnection.Database+'" on server at "'+
				AccountConnection.HostName+':'+IntToStr(AccountConnection.Port)+
				'". Error Message :: '+E.Message,
				'Database', MS_ERROR
			);
			AccountConnection.Disconnect;
			MainProc.ContinueLoading := false;
		end;
	end;

	try

		if NOT GameConnection.Connected then
		begin
			GameConnection.Connect();
		end;

	except
		on E : Exception do
		begin
			Console.Message('Could not connect to '+GameConnection.Protocol+
				' database "'+GameConnection.Database+'" on server at "'+
				GameConnection.HostName+':'+IntToStr(GameConnection.Port)+
				'". Error Message :: '+E.Message,
				'Database', MS_ERROR
			);
			GameConnection.Disconnect;
			MainProc.ContinueLoading := false;
		end;
	end;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Disconnect					                                               PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Disconnects from the database.
//
//	Changes -
//		February 1st, 2008 - RaX - Created.
//
//------------------------------------------------------------------------------
Procedure TDatabase.Disconnect(

);
begin
	if AccountConnection.Connected then
	begin
		AccountConnection.Disconnect();
	end;

	if GameConnection.Connected then
	begin
		GameConnection.Disconnect();
	end;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//LoadDatabaseOptions                                                  PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Creates and Loads the inifile.
//
//	Changes -
//		March 27th, 2007 - RaX - Created.
//
//------------------------------------------------------------------------------
Procedure TDatabase.LoadOptions;
begin
	Options    := TDatabaseOptions.Create(MainProc.Options.ConfigDirectory+'/Database.ini');

	Options.Load;
end;{LoadDatabaseOptions}
//------------------------------------------------------------------------------

end.
