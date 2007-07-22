-- Revisions pre 291 to 291 standard

-- Removed global_reg_value table.

DROP TABLE IF EXISTS global_reg_value;

-- Add new Character var table 
-- char_id and key together must be a unique combiniation.

CREATE TABLE `character_vars` (
  `char_id` int(11) NOT NULL default '0',
  `key` varchar(255) NOT NULL default '',
  `value` int(11) NOT NULL default '0',
  PRIMARY KEY  (`char_id`,`key`),
) ENGINE=MyISAM DEFAULT CHARSET=latin1;