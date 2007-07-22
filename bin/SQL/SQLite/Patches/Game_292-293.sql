-- Revisions pre 291 to 291 standard

-- Removed global_reg_value table.

DROP TABLE IF EXISTS global_reg_value;

-- Add new Character variable table 
-- char_id and key together must be a unique combiniation.

CREATE TABLE "character_vars" (
  char_id INTEGER NOT NULL default 0,
  key TEXT NOT NULL default '',
  value INTEGER NOT NULL default 0,
  PRIMARY KEY ("char_id","key")
);