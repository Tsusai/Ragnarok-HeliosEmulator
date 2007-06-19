-- Custom dump from MySQL
-- For use with SQLite only


CREATE TABLE "accounts" (
  "account_id" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL DEFAULT 100100,
  "userid" TEXT UNIQUE NOT NULL default '',
  "user_pass" TEXT NOT NULL default '',
  "lastlogin" TEXT NOT NULL default '0000-00-00 00:00:00',
  "sex" TEXT NOT NULL default 'M',
  "logincount" INTEGER NOT NULL default 0,
  "email" TEXT NOT NULL default '-@-',
  "loginkey1" INTEGER NOT NULL default 0,
  "loginkey2" INTEGER NOT NULL default 0,
  "level" INTEGER NOT NULL default 0,
  "error_message" INTEGER NOT NULL default 0,
  "connect_until" TEXT NOT NULL default '9999-12-31 23:59:59',
  "last_ip" TEXT NOT NULL default '',
  "memo" INTEGER NOT NULL default 0,
  "ban_until" TEXT NOT NULL default '0000-00-00 00:00:00',
  "state" INTEGER NOT NULL default 0
);

-- 
-- Dumping data for table "accounts"
-- 

INSERT INTO "accounts" ("account_id", "userid", "user_pass", "lastlogin", "sex", "logincount", "email", "loginkey1", "loginkey2", "level", "error_message", "connect_until", "last_ip", "memo", "ban_until", "state") VALUES 
(100100, 'test', 'test', '2007-01-07 15:43:03', 'M', 147, '-@-', 1214329757, 225113618, 1, 0, '9999-12-31 23:59:59', '127.0.0.1', 0, '1900-01-01 00:00:00', 0);
