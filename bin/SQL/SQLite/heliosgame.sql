-- Custom dump from mysql version
-- Designed for SQLite import

CREATE TABLE "cart_inventory" (
  id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
  char_id INTEGER NOT NULL default '0',
  nameid INTEGER NOT NULL default '0',
  amount INTEGER NOT NULL default '0',
  equip INTEGER NOT NULL default '0',
  identify INTEGER NOT NULL default '0',
  refine INTEGER NOT NULL default '0',
  attribute INTEGER NOT NULL default '0',
  card0 INTEGER NOT NULL default '0',
  card1 INTEGER NOT NULL default '0',
  card2 INTEGER NOT NULL default '0',
  card3 INTEGER NOT NULL default '0',
  broken INTEGER NOT NULL default '0'
);

-- 
-- Dumping data for table "cart_inventory"
-- 


-- --------------------------------------------------------

-- 
-- Table structure for table "characters"
-- 

CREATE TABLE "characters" (
  char_id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL default 53,
  account_id INTEGER NOT NULL default '0',
  char_num INTEGER NOT NULL default '0',
  name TEXT NOT NULL default '',
  class INTEGER NOT NULL default '0',
  base_level INTEGER NOT NULL default '1',
  job_level INTEGER NOT NULL default '1',
  base_exp INTEGER NOT NULL default '0',
  job_exp INTEGER NOT NULL default '0',
  zeny INTEGER NOT NULL default '500',
  p_str INTEGER NOT NULL default '0',
  p_agi INTEGER NOT NULL default '0',
  p_vit INTEGER NOT NULL default '0',
  p_int INTEGER NOT NULL default '0',
  p_dex INTEGER NOT NULL default '0',
  p_luk INTEGER NOT NULL default '0',
  max_hp INTEGER NOT NULL default '0',
  hp INTEGER NOT NULL default '0',
  max_sp INTEGER NOT NULL default '0',
  sp INTEGER NOT NULL default '0',
  status_point INTEGER NOT NULL default '0',
  skill_point INTEGER NOT NULL default '0',
  options INTEGER NOT NULL default '0',
  karma INTEGER NOT NULL default '0',
  manner INTEGER NOT NULL default '0',
  party_id INTEGER NOT NULL default '0',
  guild_id INTEGER NOT NULL default '0',
  pet_id INTEGER NOT NULL default '0',
  hair INTEGER NOT NULL default '0',
  hair_color INTEGER NOT NULL default '0',
  clothes_color INTEGER NOT NULL default '0',
  righthand INTEGER NOT NULL default '0',
  lefthand INTEGER NOT NULL default '0',
  armor INTEGER NOT NULL default '0',
  garment INTEGER NOT NULL default '0',
  shoes INTEGER NOT NULL default '0',
  accessory1 INTEGER NOT NULL default '0',
  accessory2 INTEGER NOT NULL default '0',
  head_top INTEGER NOT NULL default '0',
  head_mid INTEGER NOT NULL default '0',
  head_bottom INTEGER NOT NULL default '0',
  last_map TEXT NOT NULL default 'new_1-1.gat',
  last_x INTEGER NOT NULL default '53',
  last_y INTEGER NOT NULL default '111',
  save_map TEXT NOT NULL default 'new_1-1.gat',
  save_x INTEGER NOT NULL default '53',
  save_y INTEGER NOT NULL default '111',
  partner_id INTEGER NOT NULL default '0',
  parent_id INTEGER NOT NULL default '0',
  parent_id2 INTEGER NOT NULL default '0',
  baby_id INTEGER NOT NULL default '0',
  online INTEGER NOT NULL default '0',
  homun_id INTEGER NOT NULL default '0'
);

-- 
-- Dumping data for table "characters"
-- 


-- --------------------------------------------------------

-- 
-- Table structure for table "character_vars"
-- 

CREATE TABLE "character_vars" (
  char_id INTEGER NOT NULL default 0,
  key TEXT NOT NULL default '',
  value INTEGER NOT NULL default 0,
  PRIMARY KEY ("char_id","key")

-- 
-- Dumping data for table "character_vars"
-- 



-- --------------------------------------------------------

-- 
-- Table structure for table "friend"
-- 

CREATE TABLE "friend" (
  char_id INTEGER NOT NULL default '0',
  id1 INTEGER NOT NULL default '0',
  id2 INTEGER NOT NULL default '0',
  name TEXT NOT NULL default ''
);

-- 
-- Dumping data for table "friend"
-- 


-- --------------------------------------------------------

-- 
-- Table structure for table "guild"
-- 

CREATE TABLE "guild" (
  guild_id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL default '10000',
  name TEXT NOT NULL default '',
  master TEXT NOT NULL default '',
  guild_lv INTEGER NOT NULL default '0',
  connect_member INTEGER NOT NULL default '0',
  max_member INTEGER NOT NULL default '0',
  average_lv INTEGER NOT NULL default '0',
  exp INTEGER NOT NULL default '0',
  next_exp INTEGER NOT NULL default '0',
  skill_point INTEGER NOT NULL default '0',
  castle_id INTEGER NOT NULL default '-1',
  mes1 TEXT NOT NULL default '',
  mes2 TEXT NOT NULL default '',
  emblem_len INTEGER NOT NULL default '0',
  emblem_id INTEGER NOT NULL default '0',
  "emblem_data" blob NOT NULL
);

-- 
-- Dumping data for table "guild"
-- 


-- --------------------------------------------------------

-- 
-- Table structure for table "guild_alliance"
-- 

CREATE TABLE "guild_alliance" (
  guild_id INTEGER NOT NULL default '0',
  opposition INTEGER NOT NULL default '0',
  alliance_id INTEGER NOT NULL default '0',
  name TEXT NOT NULL default ''
);

-- 
-- Dumping data for table "guild_alliance"
-- 


-- --------------------------------------------------------

-- 
-- Table structure for table "guild_castle"
-- 

CREATE TABLE "guild_castle" (
  castle_id INTEGER NOT NULL default '0',
  guild_id INTEGER NOT NULL default '0',
  economy INTEGER NOT NULL default '0',
  defense INTEGER NOT NULL default '0',
  triggerE INTEGER NOT NULL default '0',
  triggerD INTEGER NOT NULL default '0',
  nextTime INTEGER NOT NULL default '0',
  payTime INTEGER NOT NULL default '0',
  createTime INTEGER NOT NULL default '0',
  visibleC INTEGER NOT NULL default '0',
  visibleG0 INTEGER NOT NULL default '0',
  visibleG1 INTEGER NOT NULL default '0',
  visibleG2 INTEGER NOT NULL default '0',
  visibleG3 INTEGER NOT NULL default '0',
  visibleG4 INTEGER NOT NULL default '0',
  visibleG5 INTEGER NOT NULL default '0',
  visibleG6 INTEGER NOT NULL default '0',
  visibleG7 INTEGER NOT NULL default '0',
  gHP0 INTEGER NOT NULL default '0',
  ghP1 INTEGER NOT NULL default '0',
  gHP2 INTEGER NOT NULL default '0',
  gHP3 INTEGER NOT NULL default '0',
  gHP4 INTEGER NOT NULL default '0',
  gHP5 INTEGER NOT NULL default '0',
  gHP6 INTEGER NOT NULL default '0',
  gHP7 INTEGER NOT NULL default '0',
  PRIMARY KEY  ("castle_id")
);

-- 
-- Dumping data for table "guild_castle"
-- 


-- --------------------------------------------------------

-- 
-- Table structure for table "guild_expulsion"
-- 

CREATE TABLE "guild_expulsion" (
  guild_id INTEGER NOT NULL default '0',
  name TEXT NOT NULL default '',
  mes TEXT NOT NULL default '',
  acc TEXT NOT NULL default '',
  account_id INTEGER NOT NULL default '0',
  rsv1 INTEGER NOT NULL default '0',
  rsv2 INTEGER NOT NULL default '0',
  rsv3 INTEGER NOT NULL default '0'
);

-- 
-- Dumping data for table "guild_expulsion"
-- 


-- --------------------------------------------------------

-- 
-- Table structure for table "guild_member"
-- 

CREATE TABLE "guild_member" (
  guild_id INTEGER NOT NULL default '0',
  account_id INTEGER NOT NULL default '0',
  char_id INTEGER NOT NULL default '0',
  hair INTEGER NOT NULL default '0',
  hair_color INTEGER NOT NULL default '0',
  gender INTEGER NOT NULL default '0',
  class INTEGER NOT NULL default '0',
  lv INTEGER NOT NULL default '0',
  exp INTEGER NOT NULL default '0',
  exp_payper INTEGER NOT NULL default '0',
  online INTEGER NOT NULL default '0',
  position INTEGER NOT NULL default '0',
  rsv1 INTEGER NOT NULL default '0',
  rsv2 INTEGER NOT NULL default '0',
  name TEXT NOT NULL default ''
);

-- 
-- Dumping data for table "guild_member"
-- 


-- --------------------------------------------------------

-- 
-- Table structure for table "guild_position"
-- 

CREATE TABLE "guild_position" (
  guild_id INTEGER NOT NULL default '0',
  position INTEGER NOT NULL default '0',
  name TEXT NOT NULL default '',
  mode INTEGER NOT NULL default '0',
  exp_mode INTEGER NOT NULL default '0'
);

-- 
-- Dumping data for table "guild_position"
-- 


-- --------------------------------------------------------

-- 
-- Table structure for table "guild_skill"
-- 

CREATE TABLE "guild_skill" (
  guild_id INTEGER NOT NULL default '0',
  id INTEGER NOT NULL default '0',
  lv INTEGER NOT NULL default '0'
);

-- 
-- Dumping data for table "guild_skill"
-- 


-- --------------------------------------------------------

-- 
-- Table structure for table "guild_storage"
-- 

CREATE TABLE "guild_storage" (
  id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
  guild_id INTEGER NOT NULL default '0',
  nameid INTEGER NOT NULL default '0',
  amount INTEGER NOT NULL default '0',
  equip INTEGER NOT NULL default '0',
  identify INTEGER NOT NULL default '0',
  refine INTEGER NOT NULL default '0',
  attribute INTEGER NOT NULL default '0',
  card0 INTEGER NOT NULL default '0',
  card1 INTEGER NOT NULL default '0',
  card2 INTEGER NOT NULL default '0',
  card3 INTEGER NOT NULL default '0',
  broken INTEGER NOT NULL default '0'
);

-- 
-- Dumping data for table "guild_storage"
-- 


-- --------------------------------------------------------

-- 
-- Table structure for table "homunculus"
-- 

CREATE TABLE "homunculus" (
  homun_id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
  class INTEGER NOT NULL default '0',
  name TEXT NOT NULL default '',
  account_id INTEGER NOT NULL default '0',
  char_id INTEGER NOT NULL default '0',
  base_level INTEGER NOT NULL default '0',
  base_exp INTEGER NOT NULL default '0',
  max_hp INTEGER NOT NULL default '0',
  hp INTEGER NOT NULL default '0',
  max_sp INTEGER NOT NULL default '0',
  sp INTEGER NOT NULL default '0',
  str INTEGER NOT NULL default '0',
  agi INTEGER NOT NULL default '0',
  vit INTEGER NOT NULL default '0',
  int INTEGER NOT NULL default '0',
  dex INTEGER NOT NULL default '0',
  luk INTEGER NOT NULL default '0',
  status_point INTEGER NOT NULL default '0',
  skill_point INTEGER NOT NULL default '0',
  equip INTEGER NOT NULL default '0',
  intimate INTEGER NOT NULL default '0',
  hungry INTEGER NOT NULL default '0',
  rename_flag INTEGER NOT NULL default '0',
  incubate INTEGER NOT NULL default '0'
);

-- 
-- Dumping data for table "homunculus"
-- 


-- --------------------------------------------------------

-- 
-- Table structure for table "homunculus_skill"
-- 

CREATE TABLE "homunculus_skill" (
  homun_id INTEGER NOT NULL default '0',
  id INTEGER NOT NULL default '0',
  lv INTEGER NOT NULL default '0'
);

-- 
-- Dumping data for table "homunculus_skill"
-- 


-- --------------------------------------------------------

-- 
-- Table structure for table "inventory"
-- 

CREATE TABLE "inventory" (
  id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
  char_id INTEGER NOT NULL default '0',
  nameid INTEGER NOT NULL default '0',
  amount INTEGER NOT NULL default '0',
  equip INTEGER NOT NULL default '0',
  identify INTEGER NOT NULL default '0',
  refine INTEGER NOT NULL default '0',
  attribute INTEGER NOT NULL default '0',
  card0 INTEGER NOT NULL default '0',
  card1 INTEGER NOT NULL default '0',
  card2 INTEGER NOT NULL default '0',
  card3 INTEGER NOT NULL default '0',
  broken INTEGER NOT NULL default '0'
);

-- 
-- Dumping data for table "inventory"
-- 


-- --------------------------------------------------------

-- 
-- Table structure for table "memo"
-- 

CREATE TABLE "memo" (
  memo_id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
  char_id INTEGER NOT NULL default '0',
  map TEXT NOT NULL default '',
  x INTEGER NOT NULL default '0',
  y INTEGER NOT NULL default '0'
);

-- 
-- Dumping data for table "memo"
-- 


-- --------------------------------------------------------

-- 
-- Table structure for table "party"
-- 

CREATE TABLE "party" (
  party_id INTEGER NOT NULL default '100',
  "name" TEXT NOT NULL default '',
  exp INTEGER NOT NULL default '0',
  item INTEGER NOT NULL default '0',
  leader_id INTEGER NOT NULL default '0'
);

-- 
-- Dumping data for table "party"
-- 


-- --------------------------------------------------------

-- 
-- Table structure for table "pet"
-- 

CREATE TABLE "pet" (
  pet_id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
  class INTEGER NOT NULL default '0',
  name TEXT NOT NULL default '',
  account_id INTEGER NOT NULL default '0',
  char_id INTEGER NOT NULL default '0',
  level INTEGER NOT NULL default '0',
  egg_id INTEGER NOT NULL default '0',
  equip INTEGER NOT NULL default '0',
  intimate INTEGER NOT NULL default '0',
  hungry INTEGER NOT NULL default '0',
  rename_flag INTEGER NOT NULL default '0',
  incubate INTEGER NOT NULL default '0'
);

-- 
-- Dumping data for table "pet"
-- 


-- --------------------------------------------------------

-- 
-- Table structure for table "ragsrvinfo"
-- 

CREATE TABLE "ragsrvinfo" (
  "index" INTEGER NOT NULL default '0',
  name TEXT NOT NULL default '',
  exp INTEGER NOT NULL default '0',
  jexp INTEGER NOT NULL default '0',
  "drop" INTEGER NOT NULL default '0',
  motd TEXT NOT NULL default ''
);

-- 
-- Dumping data for table "ragsrvinfo"
-- 


-- --------------------------------------------------------

-- 
-- Table structure for table "skill"
-- 

CREATE TABLE "skill" (
  char_id INTEGER NOT NULL default '0',
  id INTEGER NOT NULL default '0',
  lv INTEGER NOT NULL default '0',
  PRIMARY KEY  ("char_id","id")
);

-- 
-- Dumping data for table "skill"
-- 


-- --------------------------------------------------------

-- 
-- Table structure for table "sstatus"
-- 

CREATE TABLE "sstatus" (
  "index" INTEGER NOT NULL default '0',
  name TEXT NOT NULL default '',
  user INTEGER NOT NULL default '0'
);

-- 
-- Dumping data for table "sstatus"
-- 


-- --------------------------------------------------------

-- 
-- Table structure for table "storage"
-- 

CREATE TABLE "storage" (
  id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
  account_id INTEGER NOT NULL default '0',
  nameid INTEGER NOT NULL default '0',
  amount INTEGER NOT NULL default '0',
  equip INTEGER NOT NULL default '0',
  identify INTEGER NOT NULL default '0',
  refine INTEGER NOT NULL default '0',
  attribute INTEGER NOT NULL default '0',
  card0 INTEGER NOT NULL default '0',
  card1 INTEGER NOT NULL default '0',
  card2 INTEGER NOT NULL default '0',
  card3 INTEGER NOT NULL default '0',
  broken INTEGER NOT NULL default '0'
);

-- 
-- Dumping data for table "storage"
-- 

