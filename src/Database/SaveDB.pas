unit SaveDB;

interface
	function LoadSavedDatabase : boolean;

implementation
uses
	AccountDB;

function LoadSavedDatabase : boolean;
begin
	//Result := ; {and blah and blah} ;
	GetLastAccountIDAndCount;
end;

end.
 