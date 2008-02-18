
-----------------------------------------------------------------------------
-- Accounts
-----------------------------------------------------------------------------


CREATE TABLE [Accounts]
(
	[id] INTEGER  NOT NULL PRIMARY KEY,
	[name] CHAR(24)  NOT NULL,
	[password] CHAR(24)  NOT NULL,
	[last_login] TIMESTAMP,
	[login_count] INTEGER default 0 NOT NULL,
	[gender] CHAR(1) default 'M' NOT NULL,
	[email_address] VARCHAR(200) default '-@-' NOT NULL,
	[login_key_1] INTEGER(11),
	[login_key_2] INTEGER(11),
	[level] INTEGER(3) default 0 NOT NULL,
	[connect_until] TIMESTAMP default '9999-12-31 23:59:59',
	[banned_until] TIMESTAMP default '1899-12-30 00:00:00',
	[last_ip] CHAR(12),
	[state] INTEGER default 0 NOT NULL,
	[created_at] TIMESTAMP,
	[updated_at] TIMESTAMP
);

INSERT INTO `accounts` (`id`, `name`, `password`, `last_login`, `login_count`, `gender`, `email_address`, `login_key_1`, `login_key_2`, `level`, `connect_until`, `banned_until`, `last_ip`, `state`, `created_at`, `updated_at`) VALUES 
(100100, 'test', 'test', '2008-02-17 00:00:00', 0, 'F', '-@-', 0, 0, 255, '9999-12-31 23:59:59', '1900-01-01 00:00:00', '127.0.0.1', 0, NULL, NULL);

-----------------------------------------------------------------------------
-- AccountMemos
-----------------------------------------------------------------------------


CREATE TABLE [AccountMemos]
(
	[account_id] INTEGER,
	[memo] VARCHAR(200),
	[created_at] TIMESTAMP,
	[updated_at] TIMESTAMP,
	[id] INTEGER  NOT NULL PRIMARY KEY
);

-- SQLite does not support foreign keys; this is just for reference
-- FOREIGN KEY ([account_id]) REFERENCES Accounts ([id])
