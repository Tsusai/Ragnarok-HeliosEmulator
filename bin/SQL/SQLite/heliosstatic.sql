-- Custom dump from MySQL
-- For use with only SQLite

-- ----------------------------
-- Table structure for hp
-- ----------------------------
CREATE TABLE "hp" (
  LEVEL INTEGER default NULL,
  NOVICE INTEGER default NULL,
  SWORDMAN INTEGER default NULL,
  MAGICIAN INTEGER default NULL,
  ARCHER INTEGER default NULL,
  ACOLYTE INTEGER default NULL,
  MERCHANT INTEGER default NULL,
  THIEF INTEGER default NULL,
  KNIGHT INTEGER default NULL,
  PRIEST INTEGER default NULL,
  WIZARD INTEGER default NULL,
  BLACKSMITH INTEGER default NULL,
  HUNTER INTEGER default NULL,
  ASSASSIN INTEGER default NULL,
  CRUSADER INTEGER default NULL,
  ROGUE INTEGER default NULL,
  SAGE INTEGER default NULL,
  ALCHEMIST INTEGER default NULL,
  MONK INTEGER default NULL,
  BARD INTEGER default NULL,
  DANCER INTEGER default NULL,
  SUPER_NOVICE INTEGER default NULL,
  HIGH_NOVICE INTEGER default NULL,
  HIGH_SWORDMAN INTEGER default NULL,
  HIGH_MAGICIAN INTEGER default NULL,
  HIGH_ARCHER INTEGER default NULL,
  HIGH_ACOLYTE INTEGER default NULL,
  HIGH_MERCHANT INTEGER default NULL,
  HIGH_THIEF INTEGER default NULL,
  LORD_KNIGHT INTEGER default NULL,
  HIGH_PRIEST INTEGER default NULL,
  HIGH_WIZARD INTEGER default NULL,
  WHITESMITH INTEGER default NULL,
  SNIPER INTEGER default NULL,
  ASSASSIN_CROSS INTEGER default NULL,
  PALADIN INTEGER default NULL,
  STALKER INTEGER default NULL,
  PROFESSOR INTEGER default NULL,
  BIOCHEMIST INTEGER default NULL,
  CHAMPION INTEGER default NULL,
  CLOWN INTEGER default NULL,
  GYPSY INTEGER default NULL,
  BABY_NOVICE INTEGER default NULL,
  BABY_SWORDMAN INTEGER default NULL,
  BABY_MAGICIAN INTEGER default NULL,
  BABY_ARCHER INTEGER default NULL,
  BABY_ACOLYTE INTEGER default NULL,
  BABY_MERCHANT INTEGER default NULL,
  BABY_THIEF INTEGER default NULL,
  BABY_KNIGHT INTEGER default NULL,
  BABY_PRIEST INTEGER default NULL,
  BABY_WIZARD INTEGER default NULL,
  BABY_BLACKSMITH INTEGER default NULL,
  BABY_HUNTER INTEGER default NULL,
  BABY_ASSASSIN INTEGER default NULL,
  BABY_CRUSADER INTEGER default NULL,
  BABY_ROGUE INTEGER default NULL,
  BABY_SAGE INTEGER default NULL,
  BABY_ALCHEMIST INTEGER default NULL,
  BABY_MONK INTEGER default NULL,
  BABY_BARD INTEGER default NULL,
  BABY_DANCER INTEGER default NULL,
  SUPER_NOVICE1 INTEGER default NULL,
  TAEKWON INTEGER default NULL,
  STAR_GLADIATOR INTEGER default NULL,
  SOUL_LINKER INTEGER default NULL,
  GUNSLINGER INTEGER default NULL,
  NINJA INTEGER default NULL
);

-- ----------------------------
-- Table structure for maps
-- ----------------------------
CREATE TABLE "maps" (
  mapname TEXT NOT NULL,
  zoneid INTEGER default '1',
  memo INTEGER NOT NULL default '0',
  noreturnondc INTEGER NOT NULL default '0',
  teleport INTEGER NOT NULL default '0',
  itemdrop INTEGER NOT NULL,
  exploss INTEGER NOT NULL,
  pvp INTEGER NOT NULL default '0',
  pvpnightmare INTEGER NOT NULL default '0',
  guildpvp INTEGER NOT NULL default '0',
  items INTEGER NOT NULL default '0',
  skill INTEGER NOT NULL default '0',
  deadbranches INTEGER NOT NULL default '0',
  flywings INTEGER NOT NULL default '0',
  butterflywings INTEGER NOT NULL default '0',
  turbotrack INTEGER NOT NULL,
  noparty INTEGER NOT NULL,
  noguild INTEGER NOT NULL,
  weather INTEGER NOT NULL default '0',
  PRIMARY KEY  ("mapname")
);

-- ----------------------------
-- Table structure for sp
-- ----------------------------
CREATE TABLE "sp" (
  LEVEL INTEGER default NULL,
  NOVICE INTEGER default NULL,
  SWORDMAN INTEGER default NULL,
  MAGICIAN INTEGER default NULL,
  ARCHER INTEGER default NULL,
  ACOLYTE INTEGER default NULL,
  MERCHANT INTEGER default NULL,
  THIEF INTEGER default NULL,
  KNIGHT INTEGER default NULL,
  PRIEST INTEGER default NULL,
  WIZARD INTEGER default NULL,
  BLACKSMITH INTEGER default NULL,
  HUNTER INTEGER default NULL,
  ASSASSIN INTEGER default NULL,
  CRUSADER INTEGER default NULL,
  ROGUE INTEGER default NULL,
  SAGE INTEGER default NULL,
  ALCHEMIST INTEGER default NULL,
  MONK INTEGER default NULL,
  BARD INTEGER default NULL,
  DANCER INTEGER default NULL,
  SUPER_NOVICE INTEGER default NULL,
  HIGH_NOVICE INTEGER default NULL,
  HIGH_SWORDMAN INTEGER default NULL,
  HIGH_MAGICIAN INTEGER default NULL,
  HIGH_ARCHER INTEGER default NULL,
  HIGH_ACOLYTE INTEGER default NULL,
  HIGH_MERCHANT INTEGER default NULL,
  HIGH_THIEF INTEGER default NULL,
  LORD_KNIGHT INTEGER default NULL,
  HIGH_PRIEST INTEGER default NULL,
  HIGH_WIZARD INTEGER default NULL,
  WHITESMITH INTEGER default NULL,
  SNIPER INTEGER default NULL,
  ASSASSIN_CROSS INTEGER default NULL,
  PALADIN INTEGER default NULL,
  STALKER INTEGER default NULL,
  PROFESSOR INTEGER default NULL,
  BIOCHEMIST INTEGER default NULL,
  CHAMPION INTEGER default NULL,
  CLOWN INTEGER default NULL,
  GYPSY INTEGER default NULL,
  BABY_NOVICE INTEGER default NULL,
  BABY_SWORDMAN INTEGER default NULL,
  BABY_MAGICIAN INTEGER default NULL,
  BABY_ARCHER INTEGER default NULL,
  BABY_ACOLYTE INTEGER default NULL,
  BABY_MERCHANT INTEGER default NULL,
  BABY_THIEF INTEGER default NULL,
  BABY_KNIGHT INTEGER default NULL,
  BABY_PRIEST INTEGER default NULL,
  BABY_WIZARD INTEGER default NULL,
  BABY_BLACKSMITH INTEGER default NULL,
  BABY_HUNTER INTEGER default NULL,
  BABY_ASSASSIN INTEGER default NULL,
  BABY_CRUSADER INTEGER default NULL,
  BABY_ROGUE INTEGER default NULL,
  BABY_SAGE INTEGER default NULL,
  BABY_ALCHEMIST INTEGER default NULL,
  BABY_MONK INTEGER default NULL,
  BABY_BARD INTEGER default NULL,
  BABY_DANCER INTEGER default NULL,
  BABY_SUPER_NOVICE INTEGER default NULL,
  TAEKWON INTEGER default NULL,
  STAR_GLADIATOR INTEGER default NULL,
  SOUL_LINKER INTEGER default NULL,
  GUNSLINGER INTEGER default NULL,
  NINJA INTEGER default NULL
);

-- ----------------------------
-- Table structure for statpoints
-- ----------------------------
CREATE TABLE "statpoints" (
  level INTEGER NOT NULL,
  points INTEGER NOT NULL,
  PRIMARY KEY  ("level")
);

-- ----------------------------
-- Table structure for weight
-- ----------------------------
CREATE TABLE "weight" (
  NOVICE INTEGER default NULL,
  SWORDMAN INTEGER default NULL,
  MAGICIAN INTEGER default NULL,
  ARCHER INTEGER default NULL,
  ACOLYTE INTEGER default NULL,
  MERCHANT INTEGER default NULL,
  THIEF INTEGER default NULL,
  KNIGHT INTEGER default NULL,
  PRIEST INTEGER default NULL,
  WIZARD INTEGER default NULL,
  BLACKSMITH INTEGER default NULL,
  HUNTER INTEGER default NULL,
  ASSASSIN INTEGER default NULL,
  CRUSADER INTEGER default NULL,
  ROGUE INTEGER default NULL,
  SAGE INTEGER default NULL,
  ALCHEMIST INTEGER default NULL,
  MONK INTEGER default NULL,
  BARD INTEGER default NULL,
  DANCER INTEGER default NULL,
  SUPER_NOVICE INTEGER default NULL,
  HIGH_NOVICE INTEGER default NULL,
  HIGH_SWORDMAN INTEGER default NULL,
  HIGH_MAGICIAN INTEGER default NULL,
  HIGH_ARCHER INTEGER default NULL,
  HIGH_ACOLYTE INTEGER default NULL,
  HIGH_MERCHANT INTEGER default NULL,
  HIGH_THIEF INTEGER default NULL,
  LORD_KNIGHT INTEGER default NULL,
  HIGH_PRIEST INTEGER default NULL,
  HIGH_WIZARD INTEGER default NULL,
  WHITESMITH INTEGER default NULL,
  SNIPER INTEGER default NULL,
  ASSASSIN_CROSS INTEGER default NULL,
  PALADIN INTEGER default NULL,
  STALKER INTEGER default NULL,
  PROFESSOR INTEGER default NULL,
  BIOCHEMIST INTEGER default NULL,
  CHAMPION INTEGER default NULL,
  CLOWN INTEGER default NULL,
  GYPSY INTEGER default NULL,
  BABY_NOVICE INTEGER default NULL,
  BABY_SWORDMAN INTEGER default NULL,
  BABY_MAGICIAN INTEGER default NULL,
  BABY_ARCHER INTEGER default NULL,
  BABY_ACOLYTE INTEGER default NULL,
  BABY_MERCHANT INTEGER default NULL,
  BABY_THIEF INTEGER default NULL,
  BABY_KNIGHT INTEGER default NULL,
  BABY_PRIEST INTEGER default NULL,
  BABY_WIZARD INTEGER default NULL,
  BABY_BLACKSMITH INTEGER default NULL,
  BABY_HUNTER INTEGER default NULL,
  BABY_ASSASSIN INTEGER default NULL,
  BABY_CRUSADER INTEGER default NULL,
  BABY_ROGUE INTEGER default NULL,
  BABY_SAGE INTEGER default NULL,
  BABY_ALCHEMIST INTEGER default NULL,
  BABY_MONK INTEGER default NULL,
  BABY_BARD INTEGER default NULL,
  BABY_DANCER INTEGER default NULL,
  BABY_SUPER_NOVICE INTEGER default NULL,
  TAEKWON INTEGER default NULL,
  STAR_GLADIATOR INTEGER default NULL,
  GUNSLINGER INTEGER default NULL,
  NINJA INTEGER default NULL
);

-- ----------------------------
-- Table structure for baseexp
-- ----------------------------

CREATE TABLE `baseexp` (
  `LEVEL` smallint(6) NOT NULL default '0',
  `NOVICE` bigint(20) default NULL,
  `SWORDMAN` bigint(20) default NULL,
  `MAGICIAN` bigint(20) default NULL,
  `ARCHER` bigint(20) default NULL,
  `ACOLYTE` bigint(20) default NULL,
  `MERCHANT` bigint(20) default NULL,
  `THIEF` bigint(20) default NULL,
  `KNIGHT` bigint(20) default NULL,
  `PRIEST` bigint(20) default NULL,
  `WIZARD` bigint(20) default NULL,
  `BLACKSMITH` bigint(20) default NULL,
  `HUNTER` bigint(20) default NULL,
  `ASSASSIN` bigint(20) default NULL,
  `CRUSADER` bigint(20) default NULL,
  `ROGUE` bigint(20) default NULL,
  `SAGE` bigint(20) default NULL,
  `ALCHEMIST` bigint(20) default NULL,
  `MONK` bigint(20) default NULL,
  `BARD` bigint(20) default NULL,
  `DANCER` bigint(20) default NULL,
  `SUPER_NOVICE` bigint(20) default NULL,
  `HIGH_NOVICE` bigint(20) default NULL,
  `HIGH_SWORDMAN` bigint(20) default NULL,
  `HIGH_MAGICIAN` bigint(20) default NULL,
  `HIGH_ARCHER` bigint(20) default NULL,
  `HIGH_ACOLYTE` bigint(20) default NULL,
  `HIGH_MERCHANT` bigint(20) default NULL,
  `HIGH_THIEF` bigint(20) default NULL,
  `LORD_KNIGHT` bigint(20) default NULL,
  `HIGH_PRIEST` bigint(20) default NULL,
  `HIGH_WIZARD` bigint(20) default NULL,
  `WHITESMITH` bigint(20) default NULL,
  `SNIPER` bigint(20) default NULL,
  `ASSASSIN_CROSS` bigint(20) default NULL,
  `PALADIN` bigint(20) default NULL,
  `STALKER` bigint(20) default NULL,
  `PROFESSOR` bigint(20) default NULL,
  `BIOCHEMIST` bigint(20) default NULL,
  `CHAMPION` bigint(20) default NULL,
  `CLOWN` bigint(20) default NULL,
  `GYPSY` bigint(20) default NULL,
  `BABY_NOVICE` bigint(20) default NULL,
  `BABY_SWORDMAN` bigint(20) default NULL,
  `BABY_MAGICIAN` bigint(20) default NULL,
  `BABY_ARCHER` bigint(20) default NULL,
  `BABY_ACOLYTE` bigint(20) default NULL,
  `BABY_MERCHANT` bigint(20) default NULL,
  `BABY_THIEF` bigint(20) default NULL,
  `BABY_KNIGHT` bigint(20) default NULL,
  `BABY_PRIEST` bigint(20) default NULL,
  `BABY_WIZARD` bigint(20) default NULL,
  `BABY_BLACKSMITH` bigint(20) default NULL,
  `BABY_HUNTER` bigint(20) default NULL,
  `BABY_ASSASSIN` bigint(20) default NULL,
  `BABY_CRUSADER` bigint(20) default NULL,
  `BABY_ROGUE` bigint(20) default NULL,
  `BABY_SAGE` bigint(20) default NULL,
  `BABY_ALCHEMIST` bigint(20) default NULL,
  `BABY_MONK` bigint(20) default NULL,
  `BABY_BARD` bigint(20) default NULL,
  `BABY_DANCER` bigint(20) default NULL,
  `SUPER_NOVICE1` bigint(20) default NULL,
  `TAEKWON` bigint(20) default NULL,
  `STAR_GLADIATOR` bigint(20) default NULL,
  `SOUL_LINKER` bigint(20) default NULL,
  `GUNSLINGER` bigint(20) default NULL,
  `NINJA` bigint(20) default NULL,
  PRIMARY KEY  (`LEVEL`)
);

-- ----------------------------
-- Table structure for jobexp
-- ----------------------------

CREATE TABLE `jobexp` (
  `LEVEL` smallint(6) NOT NULL default '0',
  `NOVICE` bigint(20) default NULL,
  `SWORDMAN` bigint(20) default NULL,
  `MAGICIAN` bigint(20) default NULL,
  `ARCHER` bigint(20) default NULL,
  `ACOLYTE` bigint(20) default NULL,
  `MERCHANT` bigint(20) default NULL,
  `THIEF` bigint(20) default NULL,
  `KNIGHT` bigint(20) default NULL,
  `PRIEST` bigint(20) default NULL,
  `WIZARD` bigint(20) default NULL,
  `BLACKSMITH` bigint(20) default NULL,
  `HUNTER` bigint(20) default NULL,
  `ASSASSIN` bigint(20) default NULL,
  `CRUSADER` bigint(20) default NULL,
  `ROGUE` bigint(20) default NULL,
  `SAGE` bigint(20) default NULL,
  `ALCHEMIST` bigint(20) default NULL,
  `MONK` bigint(20) default NULL,
  `BARD` bigint(20) default NULL,
  `DANCER` bigint(20) default NULL,
  `SUPER_NOVICE` bigint(20) default NULL,
  `HIGH_NOVICE` bigint(20) default NULL,
  `HIGH_SWORDMAN` bigint(20) default NULL,
  `HIGH_MAGICIAN` bigint(20) default NULL,
  `HIGH_ARCHER` bigint(20) default NULL,
  `HIGH_ACOLYTE` bigint(20) default NULL,
  `HIGH_MERCHANT` bigint(20) default NULL,
  `HIGH_THIEF` bigint(20) default NULL,
  `LORD_KNIGHT` bigint(20) default NULL,
  `HIGH_PRIEST` bigint(20) default NULL,
  `HIGH_WIZARD` bigint(20) default NULL,
  `WHITESMITH` bigint(20) default NULL,
  `SNIPER` bigint(20) default NULL,
  `ASSASSIN_CROSS` bigint(20) default NULL,
  `PALADIN` bigint(20) default NULL,
  `STALKER` bigint(20) default NULL,
  `PROFESSOR` bigint(20) default NULL,
  `BIOCHEMIST` bigint(20) default NULL,
  `CHAMPION` bigint(20) default NULL,
  `CLOWN` bigint(20) default NULL,
  `GYPSY` bigint(20) default NULL,
  `BABY_NOVICE` bigint(20) default NULL,
  `BABY_SWORDMAN` bigint(20) default NULL,
  `BABY_MAGICIAN` bigint(20) default NULL,
  `BABY_ARCHER` bigint(20) default NULL,
  `BABY_ACOLYTE` bigint(20) default NULL,
  `BABY_MERCHANT` bigint(20) default NULL,
  `BABY_THIEF` bigint(20) default NULL,
  `BABY_KNIGHT` bigint(20) default NULL,
  `BABY_PRIEST` bigint(20) default NULL,
  `BABY_WIZARD` bigint(20) default NULL,
  `BABY_BLACKSMITH` bigint(20) default NULL,
  `BABY_HUNTER` bigint(20) default NULL,
  `BABY_ASSASSIN` bigint(20) default NULL,
  `BABY_CRUSADER` bigint(20) default NULL,
  `BABY_ROGUE` bigint(20) default NULL,
  `BABY_SAGE` bigint(20) default NULL,
  `BABY_ALCHEMIST` bigint(20) default NULL,
  `BABY_MONK` bigint(20) default NULL,
  `BABY_BARD` bigint(20) default NULL,
  `BABY_DANCER` bigint(20) default NULL,
  `SUPER_NOVICE1` bigint(20) default NULL,
  `TAEKWON` bigint(20) default NULL,
  `STAR_GLADIATOR` bigint(20) default NULL,
  `SOUL_LINKER` bigint(20) default NULL,
  `GUNSLINGER` bigint(20) default NULL,
  `NINJA` bigint(20) default NULL,
  PRIMARY KEY  (`LEVEL`)
);


-- ----------------------------
-- Records 
-- ----------------------------
INSERT INTO "hp" VALUES (null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null);
INSERT INTO "hp" VALUES ('1', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40', '40');
INSERT INTO "hp" VALUES ('2', '45', '46', '46', '46', '46', '46', '46', '48', '47', '46', '47', '47', '47', '47', '47', '47', '47', '47', '47', '47', '45', '45', '46', '46', '46', '46', '46', '46', '48', '47', '46', '47', '47', '47', '47', '47', '47', '47', '47', '47', '47', '45', '46', '46', '46', '46', '46', '46', '48', '47', '46', '47', '47', '47', '47', '47', '47', '47', '47', '47', '47', '45', '46', '47', '47', '47', '47');
INSERT INTO "hp" VALUES ('3', '50', '53', '52', '53', '52', '52', '53', '58', '54', '53', '55', '55', '55', '56', '55', '54', '55', '55', '54', '54', '50', '50', '53', '52', '53', '52', '52', '53', '58', '54', '53', '55', '55', '55', '56', '55', '54', '55', '55', '54', '54', '50', '53', '52', '53', '52', '52', '53', '58', '54', '53', '55', '55', '55', '56', '55', '54', '55', '55', '54', '54', '50', '53', '55', '54', '54', '54');
INSERT INTO "hp" VALUES ('4', '55', '61', '58', '60', '59', '59', '60', '69', '62', '60', '64', '63', '64', '62', '64', '62', '63', '64', '62', '62', '55', '55', '61', '58', '60', '59', '59', '60', '69', '62', '60', '64', '63', '64', '62', '63', '62', '64', '64', '62', '62', '55', '61', '58', '60', '59', '59', '60', '69', '62', '60', '64', '63', '64', '62', '64', '62', '63', '64', '62', '62', '55', '61', '64', '62', '61', '61');
INSERT INTO "hp" VALUES ('5', '60', '70', '65', '68', '66', '66', '68', '82', '71', '68', '74', '72', '75', '81', '74', '71', '72', '74', '71', '71', '60', '60', '70', '65', '68', '66', '66', '68', '82', '71', '68', '74', '72', '75', '81', '72', '71', '74', '74', '71', '71', '60', '70', '65', '68', '66', '66', '68', '82', '71', '68', '74', '72', '75', '81', '74', '71', '72', '74', '71', '71', '60', '70', '74', '71', '69', '69');
INSERT INTO "hp" VALUES ('6', '65', '79', '72', '76', '73', '73', '76', '96', '81', '76', '84', '82', '87', '94', '84', '81', '82', '84', '81', '81', '65', '65', '79', '72', '76', '73', '73', '76', '96', '81', '76', '84', '82', '87', '94', '82', '81', '84', '84', '81', '81', '65', '79', '72', '76', '73', '73', '76', '96', '81', '76', '84', '82', '87', '94', '84', '81', '82', '84', '81', '81', '65', '79', '84', '81', '77', '77');
INSERT INTO "hp" VALUES ('7', '70', '89', '79', '85', '81', '81', '85', '112', '91', '85', '95', '93', '100', '108', '95', '91', '93', '95', '91', '91', '70', '70', '89', '79', '85', '81', '81', '85', '112', '91', '85', '95', '93', '100', '108', '93', '91', '95', '95', '91', '91', '70', '89', '79', '85', '81', '81', '85', '112', '91', '85', '95', '93', '100', '108', '95', '91', '93', '95', '91', '91', '70', '89', '95', '91', '85', '85');
INSERT INTO "hp" VALUES ('8', '75', '100', '86', '94', '89', '89', '94', '129', '102', '94', '107', '105', '114', '128', '107', '102', '105', '107', '102', '102', '75', '75', '100', '86', '94', '89', '89', '94', '129', '102', '94', '107', '105', '114', '128', '105', '102', '107', '107', '102', '102', '75', '100', '86', '94', '89', '89', '94', '129', '102', '94', '107', '105', '114', '128', '107', '102', '105', '107', '102', '102', '75', '100', '107', '102', '94', '94');
INSERT INTO "hp" VALUES ('9', '80', '111', '94', '104', '98', '98', '104', '148', '114', '104', '120', '118', '129', '147', '120', '114', '118', '120', '114', '114', '80', '80', '111', '94', '104', '98', '98', '104', '148', '114', '104', '120', '118', '129', '147', '118', '114', '120', '120', '114', '114', '80', '111', '94', '104', '98', '98', '104', '148', '114', '104', '120', '118', '129', '147', '120', '114', '118', '120', '114', '114', '80', '111', '120', '114', '103', '103');
INSERT INTO "hp" VALUES ('10', '85', '123', '102', '114', '107', '107', '114', '168', '127', '115', '134', '132', '145', '165', '134', '127', '132', '134', '127', '127', '85', '85', '123', '102', '114', '107', '107', '114', '168', '127', '115', '134', '132', '145', '165', '132', '127', '134', '134', '127', '127', '85', '123', '102', '114', '107', '107', '114', '168', '127', '115', '134', '132', '145', '165', '134', '127', '132', '134', '127', '127', '85', '123', '134', '127', '202', '202');
INSERT INTO "hp" VALUES ('11', '90', '136', '110', '125', '116', '116', '125', '190', '140', '126', '149', '146', '162', '184', '149', '140', '146', '149', '140', '140', '90', '90', '136', '110', '125', '116', '116', '125', '190', '140', '126', '149', '146', '162', '184', '146', '140', '149', '149', '140', '140', '90', '136', '110', '125', '116', '116', '125', '190', '140', '126', '149', '146', '162', '184', '149', '140', '146', '149', '140', '140', '90', '136', '149', '140', '212', '212');
INSERT INTO "hp" VALUES ('12', '95', '149', '119', '136', '126', '126', '136', '213', '154', '138', '165', '161', '180', '204', '165', '154', '161', '165', '154', '154', '95', '95', '149', '119', '136', '126', '126', '136', '213', '154', '138', '165', '161', '180', '204', '161', '154', '165', '165', '154', '154', '95', '149', '119', '136', '126', '126', '136', '213', '154', '138', '165', '161', '180', '204', '165', '154', '161', '165', '154', '154', '95', '149', '165', '154', '222', '222');
INSERT INTO "hp" VALUES ('13', '100', '163', '128', '148', '136', '136', '148', '238', '169', '150', '182', '177', '199', '225', '182', '169', '177', '182', '169', '169', '100', '100', '163', '128', '148', '136', '136', '148', '238', '169', '150', '182', '177', '199', '225', '177', '169', '182', '182', '169', '169', '100', '163', '128', '148', '136', '136', '148', '238', '169', '150', '182', '177', '199', '225', '182', '169', '177', '182', '169', '169', '100', '163', '182', '169', '232', '232');
INSERT INTO "hp" VALUES ('14', '105', '178', '137', '160', '147', '147', '160', '264', '185', '163', '200', '194', '219', '247', '200', '185', '194', '200', '180', '180', '105', '105', '178', '137', '160', '147', '147', '160', '264', '185', '163', '200', '194', '219', '247', '194', '185', '200', '200', '180', '180', '105', '178', '137', '160', '147', '147', '160', '264', '185', '163', '200', '194', '219', '247', '200', '185', '194', '200', '180', '180', '105', '178', '200', '185', '243', '243');
INSERT INTO "hp" VALUES ('15', '110', '194', '147', '173', '158', '158', '173', '292', '201', '176', '219', '212', '241', '271', '219', '201', '212', '219', '196', '196', '110', '110', '194', '147', '173', '158', '158', '173', '292', '201', '176', '219', '212', '241', '271', '212', '201', '219', '219', '196', '196', '110', '194', '147', '173', '158', '158', '173', '292', '201', '176', '219', '212', '241', '271', '219', '201', '212', '219', '196', '196', '110', '194', '219', '201', '254', '254');
INSERT INTO "hp" VALUES ('16', '115', '210', '157', '186', '169', '169', '186', '321', '218', '190', '238', '231', '264', '296', '238', '218', '231', '238', '212', '212', '115', '115', '210', '157', '186', '169', '169', '186', '321', '218', '190', '238', '231', '264', '296', '231', '218', '238', '238', '212', '212', '115', '210', '157', '186', '169', '169', '186', '321', '218', '190', '238', '231', '264', '296', '238', '218', '231', '238', '212', '212', '115', '210', '238', '218', '265', '265');
INSERT INTO "hp" VALUES ('17', '120', '227', '167', '200', '181', '181', '200', '352', '236', '204', '258', '250', '288', '322', '258', '236', '250', '258', '224', '224', '120', '120', '227', '167', '200', '181', '181', '200', '352', '236', '204', '258', '250', '288', '322', '250', '236', '258', '258', '224', '224', '120', '227', '167', '200', '181', '181', '200', '352', '236', '204', '258', '250', '288', '322', '258', '236', '250', '258', '224', '224', '120', '227', '258', '236', '277', '277');
INSERT INTO "hp" VALUES ('18', '125', '245', '177', '214', '193', '193', '214', '384', '255', '219', '279', '270', '313', '349', '306', '255', '270', '279', '242', '242', '125', '125', '245', '177', '214', '193', '193', '214', '384', '255', '219', '279', '270', '313', '349', '270', '255', '279', '306', '242', '242', '125', '245', '177', '214', '193', '193', '214', '384', '255', '219', '279', '270', '313', '349', '306', '255', '270', '279', '242', '242', '125', '245', '306', '255', '289', '289');
INSERT INTO "hp" VALUES ('19', '130', '263', '188', '229', '206', '206', '229', '418', '274', '234', '301', '291', '339', '377', '329', '274', '291', '301', '260', '260', '130', '130', '263', '188', '229', '206', '206', '229', '418', '274', '234', '301', '291', '339', '377', '291', '274', '301', '329', '260', '260', '130', '263', '188', '229', '206', '206', '229', '418', '274', '234', '301', '291', '339', '377', '329', '274', '291', '301', '260', '260', '130', '263', '329', '274', '301', '301');
INSERT INTO "hp" VALUES ('20', '135', '282', '199', '244', '219', '219', '244', '453', '294', '250', '324', '313', '366', '406', '354', '294', '313', '324', '278', '278', '135', '135', '282', '199', '244', '219', '219', '244', '453', '294', '250', '324', '313', '366', '406', '313', '294', '324', '354', '278', '278', '135', '282', '199', '244', '219', '219', '244', '453', '294', '250', '324', '313', '366', '406', '354', '294', '313', '324', '278', '278', '135', '282', '354', '294', '316', '316');
INSERT INTO "hp" VALUES ('21', '140', '302', '210', '260', '232', '232', '260', '490', '315', '267', '348', '336', '394', '436', '379', '315', '336', '348', '296', '296', '140', '140', '302', '210', '260', '232', '232', '260', '490', '315', '267', '348', '336', '394', '436', '336', '315', '348', '379', '296', '296', '140', '302', '210', '260', '232', '232', '260', '490', '315', '267', '348', '336', '394', '436', '379', '315', '336', '348', '296', '296', '140', '302', '379', '315', '331', '331');
INSERT INTO "hp" VALUES ('22', '145', '322', '222', '276', '246', '246', '276', '528', '337', '284', '373', '360', '423', '467', '406', '337', '360', '373', '316', '316', '145', '145', '322', '222', '276', '246', '246', '276', '528', '337', '284', '373', '360', '423', '467', '360', '337', '373', '406', '316', '316', '145', '322', '222', '276', '246', '246', '276', '528', '337', '284', '373', '360', '423', '467', '406', '337', '360', '373', '316', '316', '145', '322', '406', '337', '346', '346');
INSERT INTO "hp" VALUES ('23', '150', '343', '234', '293', '260', '260', '293', '568', '359', '302', '399', '385', '453', '499', '433', '359', '385', '399', '330', '330', '150', '150', '343', '234', '293', '260', '260', '293', '568', '359', '302', '399', '385', '453', '499', '385', '359', '399', '433', '330', '330', '150', '343', '234', '293', '260', '260', '293', '568', '359', '302', '399', '385', '453', '499', '433', '359', '385', '399', '330', '330', '150', '343', '433', '359', '364', '364');
INSERT INTO "hp" VALUES ('24', '155', '365', '246', '310', '275', '275', '310', '609', '382', '320', '426', '410', '484', '532', '462', '382', '410', '426', '350', '350', '155', '155', '365', '246', '310', '275', '275', '310', '609', '382', '320', '426', '410', '484', '532', '410', '382', '426', '462', '350', '350', '155', '365', '246', '310', '275', '275', '310', '609', '382', '320', '426', '410', '484', '532', '462', '382', '410', '426', '350', '350', '155', '365', '462', '382', '382', '382');
INSERT INTO "hp" VALUES ('25', '160', '388', '259', '328', '290', '290', '328', '652', '406', '339', '454', '436', '517', '567', '491', '406', '436', '454', '371', '371', '160', '160', '388', '259', '328', '290', '290', '328', '652', '406', '339', '454', '436', '517', '567', '436', '406', '454', '491', '371', '371', '160', '388', '259', '328', '290', '290', '328', '652', '406', '339', '454', '436', '517', '567', '491', '406', '436', '454', '371', '371', '160', '388', '491', '406', '400', '400');
INSERT INTO "hp" VALUES ('26', '165', '411', '272', '346', '305', '305', '346', '696', '431', '358', '482', '463', '551', '603', '521', '431', '463', '482', '393', '393', '165', '165', '411', '272', '346', '305', '305', '346', '696', '431', '358', '482', '463', '551', '603', '463', '431', '482', '521', '393', '393', '165', '411', '272', '346', '305', '305', '346', '696', '431', '358', '482', '463', '551', '603', '521', '431', '463', '482', '393', '393', '165', '411', '521', '431', '420', '420');
INSERT INTO "hp" VALUES ('27', '170', '435', '285', '365', '321', '321', '365', '742', '456', '378', '511', '491', '586', '640', '551', '456', '491', '511', '415', '415', '170', '170', '435', '285', '365', '321', '321', '365', '742', '456', '378', '511', '491', '586', '640', '491', '456', '511', '551', '415', '415', '170', '435', '285', '365', '321', '321', '365', '742', '456', '378', '511', '491', '586', '640', '551', '456', '491', '511', '415', '415', '170', '435', '551', '456', '440', '440');
INSERT INTO "hp" VALUES ('28', '175', '460', '298', '384', '337', '337', '384', '789', '482', '398', '541', '520', '622', '678', '583', '482', '520', '541', '438', '438', '175', '175', '460', '298', '384', '337', '337', '384', '789', '482', '398', '541', '520', '622', '678', '520', '482', '541', '583', '438', '438', '175', '460', '298', '384', '337', '337', '384', '789', '482', '398', '541', '520', '622', '678', '583', '482', '520', '541', '438', '438', '175', '460', '583', '482', '460', '460');
INSERT INTO "hp" VALUES ('29', '180', '485', '312', '404', '354', '354', '404', '838', '509', '419', '572', '550', '659', '717', '615', '509', '550', '572', '451', '451', '180', '180', '485', '312', '404', '354', '354', '404', '838', '509', '419', '572', '550', '659', '717', '550', '509', '572', '615', '451', '451', '180', '485', '312', '404', '354', '354', '404', '838', '509', '419', '572', '550', '659', '717', '615', '509', '550', '572', '451', '451', '180', '485', '615', '509', '490', '482');
INSERT INTO "hp" VALUES ('30', '185', '511', '326', '424', '371', '371', '424', '888', '537', '441', '604', '581', '697', '757', '649', '537', '581', '604', '477', '477', '185', '185', '511', '326', '424', '371', '371', '424', '888', '537', '441', '604', '581', '697', '757', '581', '537', '604', '649', '477', '477', '185', '511', '326', '424', '371', '371', '424', '888', '537', '441', '604', '581', '697', '757', '649', '537', '581', '604', '477', '477', '185', '511', '649', '537', '520', '504');
INSERT INTO "hp" VALUES ('31', '190', '538', '340', '445', '388', '388', '445', '940', '565', '463', '637', '612', '736', '798', '683', '565', '612', '637', '503', '503', '190', '190', '538', '340', '445', '388', '388', '445', '940', '565', '463', '637', '612', '736', '798', '612', '565', '637', '683', '503', '503', '190', '538', '340', '445', '388', '388', '445', '940', '565', '463', '637', '612', '736', '798', '683', '565', '612', '637', '503', '503', '190', '538', '683', '565', '550', '526');
INSERT INTO "hp" VALUES ('32', '195', '565', '355', '466', '406', '406', '466', '993', '594', '486', '671', '644', '776', '840', '719', '594', '644', '671', '530', '530', '195', '195', '565', '355', '466', '406', '406', '466', '993', '594', '486', '671', '644', '776', '840', '644', '594', '671', '719', '530', '530', '195', '565', '355', '466', '406', '406', '466', '993', '594', '486', '671', '644', '776', '840', '719', '594', '644', '671', '530', '530', '195', '565', '719', '594', '580', '548');
INSERT INTO "hp" VALUES ('33', '200', '593', '370', '488', '424', '424', '488', '1048', '624', '509', '706', '677', '817', '883', '755', '624', '677', '706', '558', '558', '200', '200', '593', '370', '488', '424', '424', '488', '1048', '624', '509', '706', '677', '817', '883', '677', '624', '706', '755', '558', '558', '200', '593', '370', '488', '424', '424', '488', '1048', '624', '509', '706', '677', '817', '883', '755', '624', '677', '706', '558', '558', '200', '593', '755', '624', '610', '572');
INSERT INTO "hp" VALUES ('34', '205', '622', '385', '510', '443', '443', '510', '1104', '655', '533', '742', '711', '859', '927', '793', '655', '711', '742', '587', '587', '205', '205', '622', '385', '510', '443', '443', '510', '1104', '655', '533', '742', '711', '859', '927', '711', '655', '742', '793', '587', '587', '205', '622', '385', '510', '443', '443', '510', '1104', '655', '533', '742', '711', '859', '927', '793', '655', '711', '742', '587', '587', '205', '622', '793', '655', '650', '596');
INSERT INTO "hp" VALUES ('35', '210', '652', '401', '533', '462', '462', '533', '1162', '686', '557', '779', '746', '903', '973', '831', '686', '746', '779', '616', '616', '210', '210', '652', '401', '533', '462', '462', '533', '1162', '686', '557', '779', '746', '903', '973', '746', '686', '779', '831', '616', '616', '210', '652', '401', '533', '462', '462', '533', '1162', '686', '557', '779', '746', '903', '973', '831', '686', '746', '779', '616', '616', '210', '652', '831', '686', '680', '620');
INSERT INTO "hp" VALUES ('36', '215', '682', '417', '556', '481', '481', '556', '1221', '718', '582', '816', '782', '948', '1020', '870', '718', '782', '816', '646', '646', '215', '215', '682', '417', '556', '481', '481', '556', '1221', '718', '582', '816', '782', '948', '1020', '782', '718', '816', '870', '646', '646', '215', '682', '417', '556', '481', '481', '556', '1221', '718', '582', '816', '782', '948', '1020', '870', '718', '782', '816', '646', '646', '215', '682', '870', '718', '710', '646');
INSERT INTO "hp" VALUES ('37', '220', '713', '433', '580', '501', '501', '580', '1282', '751', '607', '854', '818', '994', '1068', '909', '751', '818', '854', '677', '677', '220', '220', '713', '433', '580', '501', '501', '580', '1282', '751', '607', '854', '818', '994', '1068', '818', '751', '854', '909', '677', '677', '220', '713', '433', '580', '501', '501', '580', '1282', '751', '607', '854', '818', '994', '1068', '909', '751', '818', '854', '677', '677', '220', '713', '909', '751', '740', '672');
INSERT INTO "hp" VALUES ('38', '225', '745', '449', '604', '521', '521', '604', '1344', '785', '633', '893', '855', '1041', '1117', '950', '785', '855', '893', '709', '709', '225', '225', '745', '449', '604', '521', '521', '604', '1344', '785', '633', '893', '855', '1041', '1117', '855', '785', '893', '950', '709', '709', '225', '745', '449', '604', '521', '521', '604', '1344', '785', '633', '893', '855', '1041', '1117', '950', '785', '855', '893', '709', '709', '225', '745', '950', '785', '770', '698');
INSERT INTO "hp" VALUES ('39', '230', '777', '466', '629', '542', '542', '629', '1408', '819', '659', '933', '893', '1089', '1167', '991', '819', '893', '933', '741', '741', '230', '230', '777', '466', '629', '542', '542', '629', '1408', '819', '659', '933', '893', '1089', '1167', '893', '819', '933', '991', '741', '741', '230', '777', '466', '629', '542', '542', '629', '1408', '819', '659', '933', '893', '1089', '1167', '991', '819', '893', '933', '741', '741', '230', '777', '991', '819', '800', '726');
INSERT INTO "hp" VALUES ('40', '235', '810', '483', '654', '563', '563', '654', '1473', '854', '686', '974', '932', '1138', '1218', '1034', '854', '932', '974', '774', '774', '235', '235', '810', '483', '654', '563', '563', '654', '1473', '854', '686', '974', '932', '1138', '1218', '932', '854', '974', '1034', '774', '774', '235', '810', '483', '654', '563', '563', '654', '1473', '854', '686', '974', '932', '1138', '1218', '1034', '854', '932', '974', '774', '774', '235', '810', '1034', '854', '830', '754');
INSERT INTO "hp" VALUES ('41', '240', '844', '500', '680', '584', '584', '680', '1540', '890', '714', '1016', '972', '1188', '1270', '1077', '890', '972', '1016', '808', '808', '240', '240', '844', '500', '680', '584', '584', '680', '1540', '890', '714', '1016', '972', '1188', '1270', '972', '890', '1016', '1077', '808', '808', '240', '844', '500', '680', '584', '584', '680', '1540', '890', '714', '1016', '972', '1188', '1270', '1077', '890', '972', '1016', '808', '808', '240', '844', '1077', '890', '865', '784');
INSERT INTO "hp" VALUES ('42', '245', '878', '518', '706', '606', '606', '706', '1608', '927', '742', '1059', '1013', '1239', '1323', '1122', '927', '1013', '1059', '843', '843', '245', '245', '878', '518', '706', '606', '606', '706', '1608', '927', '742', '1059', '1013', '1239', '1323', '1013', '927', '1059', '1122', '843', '843', '245', '878', '518', '706', '606', '606', '706', '1608', '927', '742', '1059', '1013', '1239', '1323', '1122', '927', '1013', '1059', '843', '843', '245', '878', '1122', '927', '890', '814');
INSERT INTO "hp" VALUES ('43', '250', '913', '536', '733', '628', '628', '733', '1678', '964', '771', '1103', '1055', '1291', '1377', '1167', '964', '1055', '1103', '878', '878', '250', '250', '913', '536', '733', '628', '628', '733', '1678', '964', '771', '1103', '1055', '1291', '1377', '1055', '964', '1103', '1167', '878', '878', '250', '913', '536', '733', '628', '628', '733', '1678', '964', '771', '1103', '1055', '1291', '1377', '1167', '964', '1055', '1103', '878', '878', '250', '913', '1167', '964', '925', '844');
INSERT INTO "hp" VALUES ('44', '255', '949', '554', '760', '651', '651', '760', '1749', '1002', '800', '1148', '1097', '1344', '1432', '1214', '1002', '1097', '1148', '914', '914', '255', '255', '949', '554', '760', '651', '651', '760', '1749', '1002', '800', '1148', '1097', '1344', '1432', '1097', '1002', '1148', '1214', '914', '914', '255', '949', '554', '760', '651', '651', '760', '1749', '1002', '800', '1148', '1097', '1344', '1432', '1214', '1002', '1097', '1148', '914', '914', '255', '949', '1214', '1002', '955', '876');
INSERT INTO "hp" VALUES ('45', '260', '986', '573', '788', '674', '674', '788', '1822', '1041', '830', '1194', '1140', '1399', '1489', '1261', '1041', '1140', '1194', '951', '951', '260', '260', '986', '573', '788', '674', '674', '788', '1822', '1041', '830', '1194', '1140', '1399', '1489', '1140', '1041', '1194', '1261', '951', '951', '260', '986', '573', '788', '674', '674', '788', '1822', '1041', '830', '1194', '1140', '1399', '1489', '1261', '1041', '1140', '1194', '951', '951', '260', '986', '1261', '1041', '990', '908');
INSERT INTO "hp" VALUES ('46', '265', '1023', '592', '816', '697', '697', '816', '1896', '1081', '860', '1240', '1184', '1455', '1547', '1309', '1081', '1184', '1240', '989', '989', '265', '265', '1023', '592', '816', '697', '697', '816', '1896', '1081', '860', '1240', '1184', '1455', '1547', '1184', '1081', '1240', '1309', '989', '989', '265', '1023', '592', '816', '697', '697', '816', '1896', '1081', '860', '1240', '1184', '1455', '1547', '1309', '1081', '1184', '1240', '989', '989', '265', '1023', '1309', '1081', '1025', '940');
INSERT INTO "hp" VALUES ('47', '270', '1061', '611', '845', '721', '721', '845', '1972', '1121', '891', '1287', '1229', '1512', '1606', '1357', '1121', '1229', '1287', '1027', '1027', '270', '270', '1061', '611', '845', '721', '721', '845', '1972', '1121', '891', '1287', '1229', '1512', '1606', '1229', '1121', '1287', '1357', '1027', '1027', '270', '1061', '611', '845', '721', '721', '845', '1972', '1121', '891', '1287', '1229', '1512', '1606', '1357', '1121', '1229', '1287', '1027', '1027', '270', '1061', '1357', '1121', '1050', '975');
INSERT INTO "hp" VALUES ('48', '275', '1100', '630', '874', '745', '745', '874', '2049', '1162', '922', '1335', '1275', '1570', '1666', '1407', '1162', '1275', '1335', '1066', '1066', '275', '275', '1100', '630', '874', '745', '745', '874', '2049', '1162', '922', '1335', '1275', '1570', '1666', '1275', '1162', '1335', '1407', '1066', '1066', '275', '1100', '630', '874', '745', '745', '874', '2049', '1162', '922', '1335', '1275', '1570', '1666', '1407', '1162', '1275', '1335', '1066', '1066', '275', '1100', '1407', '1162', '1080', '1010');
INSERT INTO "hp" VALUES ('49', '280', '1139', '650', '904', '770', '770', '904', '2128', '1204', '954', '1384', '1322', '1629', '1727', '1457', '1204', '1322', '1384', '1106', '1106', '280', '280', '1139', '650', '904', '770', '770', '904', '2128', '1204', '954', '1384', '1322', '1629', '1727', '1322', '1204', '1384', '1457', '1106', '1106', '280', '1139', '650', '904', '770', '770', '904', '2128', '1204', '954', '1384', '1322', '1629', '1727', '1457', '1204', '1322', '1384', '1106', '1106', '280', '1139', '1457', '1204', '1110', '1100');
INSERT INTO "hp" VALUES ('50', '285', '1179', '670', '934', '795', '795', '934', '2208', '1247', '987', '1434', '1370', '1689', '1789', '1509', '1247', '1370', '1434', '1147', '1147', '285', '285', '1179', '670', '934', '795', '795', '934', '2208', '1247', '987', '1434', '1370', '1689', '1789', '1370', '1247', '1434', '1509', '1147', '1147', '285', '1179', '670', '934', '795', '795', '934', '2208', '1247', '987', '1434', '1370', '1689', '1789', '1509', '1247', '1370', '1434', '1147', '1147', '285', '1179', '1509', '1247', '1145', '1140');
INSERT INTO "hp" VALUES ('51', '290', '1220', '690', '965', '820', '820', '965', '2290', '1290', '1020', '1485', '1418', '1750', '1852', '1561', '1290', '1418', '1485', '1188', '1188', '290', '290', '1220', '690', '965', '820', '820', '965', '2290', '1290', '1020', '1485', '1418', '1750', '1852', '1418', '1290', '1485', '1561', '1188', '1188', '290', '1220', '690', '965', '820', '820', '965', '2290', '1290', '1020', '1485', '1418', '1750', '1852', '1561', '1290', '1418', '1485', '1188', '1188', '290', '1220', '1561', '1290', '1180', '1180');
INSERT INTO "hp" VALUES ('52', '295', '1261', '711', '996', '846', '846', '996', '2373', '1334', '1054', '1537', '1467', '1812', '1916', '1615', '1334', '1467', '1537', '1230', '1230', '295', '295', '1261', '711', '996', '846', '846', '996', '2373', '1334', '1054', '1537', '1467', '1812', '1916', '1467', '1334', '1537', '1615', '1230', '1230', '295', '1261', '711', '996', '846', '846', '996', '2373', '1334', '1054', '1537', '1467', '1812', '1916', '1615', '1334', '1467', '1537', '1230', '1230', '295', '1261', '1615', '1334', '1215', '1220');
INSERT INTO "hp" VALUES ('53', '300', '1303', '732', '1028', '872', '872', '1028', '2458', '1379', '1088', '1590', '1517', '1875', '1981', '1669', '1379', '1517', '1590', '1273', '1273', '300', '300', '1303', '732', '1028', '872', '872', '1028', '2458', '1379', '1088', '1590', '1517', '1875', '1981', '1517', '1379', '1590', '1669', '1273', '1273', '300', '1303', '732', '1028', '872', '872', '1028', '2458', '1379', '1088', '1590', '1517', '1875', '1981', '1669', '1379', '1517', '1590', '1273', '1273', '300', '1303', '1669', '1379', '1275', '1260');
INSERT INTO "hp" VALUES ('54', '305', '1346', '753', '1060', '899', '899', '1060', '2544', '1425', '1123', '1644', '1568', '1939', '2047', '1725', '1425', '1568', '1644', '1317', '1317', '305', '305', '1346', '753', '1060', '899', '899', '1060', '2544', '1425', '1123', '1644', '1568', '1939', '2047', '1568', '1425', '1644', '1725', '1317', '1317', '305', '1346', '753', '1060', '899', '899', '1060', '2544', '1425', '1123', '1644', '1568', '1939', '2047', '1725', '1425', '1568', '1644', '1317', '1317', '305', '1346', '1725', '1425', '1335', '1300');
INSERT INTO "hp" VALUES ('55', '310', '1390', '775', '1093', '926', '926', '1093', '2632', '1471', '1158', '1699', '1620', '2005', '2115', '1781', '1471', '1620', '1699', '1361', '1361', '310', '310', '1390', '775', '1093', '926', '926', '1093', '2632', '1471', '1158', '1699', '1620', '2005', '2115', '1620', '1471', '1699', '1781', '1361', '1361', '310', '1390', '775', '1093', '926', '926', '1093', '2632', '1471', '1158', '1699', '1620', '2005', '2115', '1781', '1471', '1620', '1699', '1361', '1361', '310', '1390', '1781', '1471', '1395', '1340');
INSERT INTO "hp" VALUES ('56', '315', '1434', '797', '1126', '953', '953', '1126', '2721', '1518', '1194', '1754', '1673', '2072', '2184', '1838', '1518', '1673', '1754', '1406', '1406', '315', '315', '1434', '797', '1126', '953', '953', '1126', '2721', '1518', '1194', '1754', '1673', '2072', '2184', '1673', '1518', '1754', '1838', '1406', '1406', '315', '1434', '797', '1126', '953', '953', '1126', '2721', '1518', '1194', '1754', '1673', '2072', '2184', '1838', '1518', '1673', '1754', '1406', '1406', '315', '1434', '1838', '1518', '1455', '1385');
INSERT INTO "hp" VALUES ('57', '320', '1479', '819', '1160', '981', '981', '1160', '2812', '1566', '1230', '1810', '1726', '2140', '2254', '1895', '1566', '1726', '1810', '1452', '1452', '320', '320', '1479', '819', '1160', '981', '981', '1160', '2812', '1566', '1230', '1810', '1726', '2140', '2254', '1726', '1566', '1810', '1895', '1452', '1452', '320', '1479', '819', '1160', '981', '981', '1160', '2812', '1566', '1230', '1810', '1726', '2140', '2254', '1895', '1566', '1726', '1810', '1452', '1452', '320', '1479', '1895', '1566', '1515', '1430');
INSERT INTO "hp" VALUES ('58', '325', '1525', '841', '1194', '1009', '1009', '1194', '2904', '1615', '1267', '1867', '1780', '2209', '2325', '1954', '1615', '1780', '1867', '1499', '1499', '325', '325', '1525', '841', '1194', '1009', '1009', '1194', '2904', '1615', '1267', '1867', '1780', '2209', '2325', '1780', '1615', '1867', '1954', '1499', '1499', '325', '1525', '841', '1194', '1009', '1009', '1194', '2904', '1615', '1267', '1867', '1780', '2209', '2325', '1954', '1615', '1780', '1867', '1499', '1499', '325', '1525', '1954', '1615', '1575', '1475');
INSERT INTO "hp" VALUES ('59', '330', '1571', '864', '1229', '1038', '1038', '1229', '2998', '1664', '1304', '1925', '1835', '2279', '2397', '2013', '1664', '1835', '1925', '1546', '1546', '330', '330', '1571', '864', '1229', '1038', '1038', '1229', '2998', '1664', '1304', '1925', '1835', '2279', '2397', '1835', '1664', '1925', '2013', '1546', '1546', '330', '1571', '864', '1229', '1038', '1038', '1229', '2998', '1664', '1304', '1925', '1835', '2279', '2397', '2013', '1664', '1835', '1925', '1546', '1546', '330', '1571', '2013', '1664', '1635', '1520');
INSERT INTO "hp" VALUES ('60', '335', '1618', '887', '1264', '1067', '1067', '1264', '3093', '1714', '1342', '1984', '1891', '2350', '2470', '2074', '1714', '1891', '1984', '1594', '1594', '335', '335', '1618', '887', '1264', '1067', '1067', '1264', '3093', '1714', '1342', '1984', '1891', '2350', '2470', '1891', '1714', '1984', '2074', '1594', '1594', '335', '1618', '887', '1264', '1067', '1067', '1264', '3093', '1714', '1342', '1984', '1891', '2350', '2470', '2074', '1714', '1891', '1984', '1594', '1594', '335', '1618', '2074', '1714', '1695', '1565');
INSERT INTO "hp" VALUES ('61', '340', '1666', '910', '1300', '1096', '1096', '1300', '3190', '1765', '1381', '2044', '1948', '2422', '2544', '2135', '1765', '1948', '2044', '1643', '1643', '340', '340', '1666', '910', '1300', '1096', '1096', '1300', '3190', '1765', '1381', '2044', '1948', '2422', '2544', '1948', '1765', '2044', '2135', '1643', '1643', '340', '1666', '910', '1300', '1096', '1096', '1300', '3190', '1765', '1381', '2044', '1948', '2422', '2544', '2135', '1765', '1948', '2044', '1643', '1643', '340', '1666', '2135', '1765', '1760', '1615');
INSERT INTO "hp" VALUES ('62', '345', '1714', '934', '1336', '1126', '1126', '1336', '3288', '1817', '1420', '2105', '2006', '2495', '2619', '2198', '1817', '2006', '2105', '1693', '1693', '345', '345', '1714', '934', '1336', '1126', '1126', '1336', '3288', '1817', '1420', '2105', '2006', '2495', '2619', '2006', '1817', '2105', '2198', '1693', '1693', '345', '1714', '934', '1336', '1126', '1126', '1336', '3288', '1817', '1420', '2105', '2006', '2495', '2619', '2198', '1817', '2006', '2105', '1693', '1693', '345', '1714', '2198', '1817', '1820', '1665');
INSERT INTO "hp" VALUES ('63', '350', '1763', '958', '1373', '1156', '1156', '1373', '3388', '1869', '1460', '2167', '2065', '2569', '2695', '2261', '1869', '2065', '2167', '1743', '1743', '350', '350', '1763', '958', '1373', '1156', '1156', '1373', '3388', '1869', '1460', '2167', '2065', '2569', '2695', '2065', '1869', '2167', '2261', '1743', '1743', '350', '1763', '958', '1373', '1156', '1156', '1373', '3388', '1869', '1460', '2167', '2065', '2569', '2695', '2261', '1869', '2065', '2167', '1743', '1743', '350', '1763', '2261', '1869', '1885', '1715');
INSERT INTO "hp" VALUES ('64', '355', '1813', '982', '1410', '1187', '1187', '1410', '3489', '1922', '1500', '2230', '2124', '2644', '2772', '2326', '1922', '2124', '2230', '1794', '1794', '355', '355', '1813', '982', '1410', '1187', '1187', '1410', '3489', '1922', '1500', '2230', '2124', '2644', '2772', '2124', '1922', '2230', '2326', '1794', '1794', '355', '1813', '982', '1410', '1187', '1187', '1410', '3489', '1922', '1500', '2230', '2124', '2644', '2772', '2326', '1922', '2124', '2230', '1794', '1794', '355', '1813', '2326', '1922', '1950', '1765');
INSERT INTO "hp" VALUES ('65', '360', '1864', '1007', '1448', '1218', '1218', '1448', '3592', '1976', '1541', '2294', '2184', '2721', '2851', '2391', '1976', '2184', '2294', '1846', '1846', '360', '360', '1864', '1007', '1448', '1218', '1218', '1448', '3592', '1976', '1541', '2294', '2184', '2721', '2851', '2184', '1976', '2294', '2391', '1846', '1846', '360', '1864', '1007', '1448', '1218', '1218', '1448', '3592', '1976', '1541', '2294', '2184', '2721', '2851', '2391', '1976', '2184', '2294', '1846', '1846', '360', '1864', '2391', '1976', '2015', '1815');
INSERT INTO "hp" VALUES ('66', '365', '1915', '1032', '1486', '1249', '1249', '1486', '3696', '2031', '1582', '2358', '2245', '2799', '2931', '2457', '2031', '2245', '2358', '1899', '1899', '365', '365', '1915', '1032', '1486', '1249', '1249', '1486', '3696', '2031', '1582', '2358', '2245', '2799', '2931', '2245', '2031', '2358', '2457', '1899', '1899', '365', '1915', '1032', '1486', '1249', '1249', '1486', '3696', '2031', '1582', '2358', '2245', '2799', '2931', '2457', '2031', '2245', '2358', '1899', '1899', '365', '1915', '2457', '2031', '2080', '1880');
INSERT INTO "hp" VALUES ('67', '370', '1967', '1057', '1525', '1281', '1281', '1525', '3802', '2086', '1624', '2423', '2307', '2878', '3012', '2523', '2086', '2307', '2423', '1952', '1952', '370', '370', '1967', '1057', '1525', '1281', '1281', '1525', '3802', '2086', '1624', '2423', '2307', '2878', '3012', '2307', '2086', '2423', '2523', '1952', '1952', '370', '1967', '1057', '1525', '1281', '1281', '1525', '3802', '2086', '1624', '2423', '2307', '2878', '3012', '2523', '2086', '2307', '2423', '1952', '1952', '370', '1967', '2523', '2086', '2145', '1935');
INSERT INTO "hp" VALUES ('68', '375', '2020', '1082', '1564', '1313', '1313', '1564', '3909', '2142', '1666', '2489', '2370', '2958', '3094', '2591', '2142', '2370', '2489', '2006', '2006', '375', '375', '2020', '1082', '1564', '1313', '1313', '1564', '3909', '2142', '1666', '2489', '2370', '2958', '3094', '2370', '2142', '2489', '2591', '2006', '2006', '375', '2020', '1082', '1564', '1313', '1313', '1564', '3909', '2142', '1666', '2489', '2370', '2958', '3094', '2591', '2142', '2370', '2489', '2006', '2006', '375', '2020', '2591', '2142', '2210', '1990');
INSERT INTO "hp" VALUES ('69', '380', '2073', '1108', '1604', '1346', '1346', '1604', '4018', '2199', '1709', '2556', '2434', '3039', '3177', '2659', '2199', '2434', '2556', '2061', '2061', '380', '380', '2073', '1108', '1604', '1346', '1346', '1604', '4018', '2199', '1709', '2556', '2434', '3039', '3177', '2434', '2199', '2556', '2659', '2061', '2061', '380', '2073', '1108', '1604', '1346', '1346', '1604', '4018', '2199', '1709', '2556', '2434', '3039', '3177', '2659', '2199', '2434', '2556', '2061', '2061', '380', '2073', '2659', '2199', '2275', '2045');
INSERT INTO "hp" VALUES ('70', '385', '2127', '1134', '1644', '1379', '1379', '1644', '4128', '2257', '1753', '2624', '2499', '3121', '3261', '2729', '2257', '2499', '2624', '2117', '2117', '385', '385', '2127', '1134', '1644', '1379', '1379', '1644', '4128', '2257', '1753', '2624', '2499', '3121', '3261', '2499', '2257', '2624', '2729', '2117', '2117', '385', '2127', '1134', '1644', '1379', '1379', '1644', '4128', '2257', '1753', '2624', '2499', '3121', '3261', '2729', '2257', '2499', '2624', '2117', '2117', '385', '2127', '2670', '2257', '2340', '2100');
INSERT INTO "hp" VALUES ('71', '390', '2182', '1160', '1685', '1412', '1412', '1685', '4240', '2315', '1797', '2693', '2564', '3204', '3346', '2799', '2315', '2564', '2693', '2173', '2173', '390', '390', '2182', '1160', '1685', '1412', '1412', '1685', '4240', '2315', '1797', '2693', '2564', '3204', '3346', '2564', '2315', '2693', '2799', '2173', '2173', '390', '2182', '1160', '1685', '1412', '1412', '1685', '4240', '2315', '1797', '2693', '2564', '3204', '3346', '2799', '2315', '2564', '2693', '2173', '2173', '390', '2137', '2680', '2275', '2410', '2160');
INSERT INTO "hp" VALUES ('72', '395', '2237', '1187', '1726', '1446', '1446', '1726', '4353', '2374', '1842', '2763', '2630', '3288', '3432', '2871', '2374', '2630', '2763', '2230', '2230', '395', '395', '2237', '1187', '1726', '1446', '1446', '1726', '4353', '2374', '1842', '2763', '2630', '3288', '3432', '2630', '2374', '2763', '2871', '2230', '2230', '395', '2237', '1187', '1726', '1446', '1446', '1726', '4353', '2374', '1842', '2763', '2630', '3288', '3432', '2871', '2374', '2630', '2763', '2230', '2230', '395', '2147', '2690', '2294', '2480', '2220');
INSERT INTO "hp" VALUES ('73', '400', '2293', '1214', '1768', '1480', '1480', '1768', '4468', '2434', '1887', '2834', '2697', '3373', '3519', '2943', '2434', '2697', '2834', '2288', '2288', '400', '400', '2293', '1214', '1768', '1480', '1480', '1768', '4468', '2434', '1887', '2834', '2697', '3373', '3519', '2697', '2434', '2834', '2943', '2288', '2288', '400', '2293', '1214', '1768', '1480', '1480', '1768', '4468', '2434', '1887', '2834', '2697', '3373', '3519', '2943', '2434', '2697', '2834', '2288', '2288', '400', '2157', '2700', '2314', '2550', '2280');
INSERT INTO "hp" VALUES ('74', '405', '2350', '1241', '1810', '1515', '1515', '1810', '4584', '2495', '1933', '2906', '2765', '3459', '3607', '3017', '2495', '2765', '2906', '2347', '2347', '405', '405', '2350', '1241', '1810', '1515', '1515', '1810', '4584', '2495', '1933', '2906', '2765', '3459', '3607', '2765', '2495', '2906', '3017', '2347', '2347', '405', '2350', '1241', '1810', '1515', '1515', '1810', '4584', '2495', '1933', '2906', '2765', '3459', '3607', '3017', '2495', '2765', '2906', '2347', '2347', '405', '2167', '2710', '2335', '2620', '2340');
INSERT INTO "hp" VALUES ('75', '410', '2408', '1269', '1853', '1550', '1550', '1853', '4702', '2556', '1979', '2979', '2834', '3547', '3697', '3091', '2556', '2834', '2979', '2406', '2406', '410', '410', '2408', '1269', '1853', '1550', '1550', '1853', '4702', '2556', '1979', '2979', '2834', '3547', '3697', '2834', '2556', '2979', '3091', '2406', '2406', '410', '2408', '1269', '1853', '1550', '1550', '1853', '4702', '2556', '1979', '2979', '2834', '3547', '3697', '3091', '2556', '2834', '2979', '2406', '2406', '410', '2177', '2720', '2356', '2690', '2400');
INSERT INTO "hp" VALUES ('76', '415', '2466', '1297', '1896', '1585', '1585', '1896', '4821', '2618', '2026', '3052', '2904', '3636', '3788', '3166', '2618', '2904', '3052', '2466', '2466', '415', '415', '2466', '1297', '1896', '1585', '1585', '1896', '4821', '2618', '2026', '3052', '2904', '3636', '3788', '2904', '2618', '3052', '3166', '2466', '2466', '415', '2466', '1297', '1896', '1585', '1585', '1896', '4821', '2618', '2026', '3052', '2904', '3636', '3788', '3166', '2618', '2904', '3052', '2466', '2466', '415', '2187', '2730', '2378', '2760', '2460');
INSERT INTO "hp" VALUES ('77', '420', '2525', '1325', '1940', '1621', '1621', '1940', '4942', '2681', '2073', '3126', '2974', '3726', '3880', '3241', '2681', '2974', '3126', '2527', '2527', '420', '420', '2525', '1325', '1940', '1621', '1621', '1940', '4942', '2681', '2073', '3126', '2974', '3726', '3880', '2974', '2681', '3126', '3241', '2527', '2527', '420', '2525', '1325', '1940', '1621', '1621', '1940', '4942', '2681', '2073', '3126', '2974', '3726', '3880', '3241', '2681', '2974', '3126', '2527', '2527', '420', '2197', '2740', '2401', '2830', '2520');
INSERT INTO "hp" VALUES ('78', '425', '2585', '1353', '1984', '1657', '1657', '1984', '5064', '2745', '2121', '3201', '3045', '3817', '3973', '3318', '2745', '3045', '3201', '2589', '2589', '425', '425', '2585', '1353', '1984', '1657', '1657', '1984', '5064', '2745', '2121', '3201', '3045', '3817', '3973', '3045', '2745', '3201', '3318', '2589', '2589', '425', '2585', '1353', '1984', '1657', '1657', '1984', '5064', '2745', '2121', '3201', '3045', '3817', '3973', '3318', '2745', '3045', '3201', '2589', '2589', '425', '2217', '2750', '2425', '2900', '2580');
INSERT INTO "hp" VALUES ('79', '430', '2645', '1382', '2029', '1694', '1694', '2029', '5188', '2809', '2169', '3277', '3117', '3909', '4067', '3395', '2809', '3117', '3277', '2651', '2651', '430', '430', '2645', '1382', '2029', '1694', '1694', '2029', '5188', '2809', '2169', '3277', '3117', '3909', '4067', '3117', '2809', '3277', '3395', '2651', '2651', '430', '2645', '1382', '2029', '1694', '1694', '2029', '5188', '2809', '2169', '3277', '3117', '3909', '4067', '3395', '2809', '3117', '3277', '2651', '2651', '430', '2127', '2760', '2449', '2970', '2640');
INSERT INTO "hp" VALUES ('80', '435', '2706', '1411', '2074', '1731', '1731', '2074', '5313', '2874', '2218', '3354', '3190', '4002', '4162', '3474', '2874', '3190', '3354', '2714', '2714', '435', '435', '2706', '1411', '2074', '1731', '1731', '2074', '5313', '2874', '2218', '3354', '3190', '4002', '4162', '3190', '2874', '3354', '3474', '2714', '2714', '435', '2706', '1411', '2074', '1731', '1731', '2074', '5313', '2874', '2218', '3354', '3190', '4002', '4162', '3474', '2874', '3190', '3354', '2714', '2714', '435', '2200', '3000', '2874', '3040', '2705');
INSERT INTO "hp" VALUES ('81', '440', '2768', '1440', '2120', '1768', '1768', '2120', '5440', '2940', '2268', '3432', '3264', '4096', '4258', '3553', '2940', '3264', '3432', '2778', '2778', '440', '440', '2768', '1440', '2120', '1768', '1768', '2120', '5440', '2940', '2268', '3432', '3264', '4096', '4258', '3264', '2940', '3432', '3553', '2778', '2778', '440', '2768', '1440', '2120', '1768', '1768', '2120', '5440', '2940', '2268', '3432', '3264', '4096', '4258', '3553', '2940', '3264', '3432', '2778', '2778', '440', '2250', '3020', '2890', '3115', '2770');
INSERT INTO "hp" VALUES ('82', '445', '2830', '1470', '2166', '1806', '1806', '2166', '5568', '3007', '2318', '3511', '3339', '4191', '4355', '3634', '3007', '3339', '3511', '2843', '2843', '445', '445', '2830', '1470', '2166', '1806', '1806', '2166', '5568', '3007', '2318', '3511', '3339', '4191', '4355', '3339', '3007', '3511', '3634', '2843', '2843', '445', '2830', '1470', '2166', '1806', '1806', '2166', '5568', '3007', '2318', '3511', '3339', '4191', '4355', '3634', '3007', '3339', '3511', '2843', '2843', '445', '2300', '3040', '2907', '3190', '2835');
INSERT INTO "hp" VALUES ('83', '450', '2893', '1500', '2213', '1844', '1844', '2213', '5698', '3074', '2369', '3591', '3415', '4287', '4453', '3715', '3074', '3415', '3591', '2908', '2908', '450', '450', '2893', '1500', '2213', '1844', '1844', '2213', '5698', '3074', '2369', '3591', '3415', '4287', '4453', '3415', '3074', '3591', '3715', '2908', '2908', '450', '2893', '1500', '2213', '1844', '1844', '2213', '5698', '3074', '2369', '3591', '3415', '4287', '4453', '3715', '3074', '3415', '3591', '2908', '2908', '450', '2350', '3060', '2924', '3265', '2900');
INSERT INTO "hp" VALUES ('84', '455', '2957', '1530', '2260', '1883', '1883', '2260', '5829', '3142', '2420', '3672', '3491', '4384', '4552', '3798', '3142', '3491', '3672', '2974', '2974', '455', '455', '2957', '1530', '2260', '1883', '1883', '2260', '5829', '3142', '2420', '3672', '3491', '4384', '4552', '3491', '3142', '3672', '3798', '2974', '2974', '455', '2957', '1530', '2260', '1883', '1883', '2260', '5829', '3142', '2420', '3672', '3491', '4384', '4552', '3798', '3142', '3491', '3672', '2974', '2974', '455', '2400', '3080', '2942', '3340', '2965');
INSERT INTO "hp" VALUES ('85', '460', '3022', '1561', '2308', '1922', '1922', '2308', '5962', '3211', '2472', '3754', '3568', '4483', '4653', '3881', '3211', '3568', '3754', '3041', '3041', '460', '460', '3022', '1561', '2308', '1922', '1922', '2308', '5962', '3211', '2472', '3754', '3568', '4483', '4653', '3568', '3211', '3754', '3881', '3041', '3041', '460', '3022', '1561', '2308', '1922', '1922', '2308', '5962', '3211', '2472', '3754', '3568', '4483', '4653', '3881', '3211', '3568', '3754', '3041', '3041', '460', '2450', '3100', '2971', '3415', '3030');
INSERT INTO "hp" VALUES ('86', '465', '3087', '1592', '2356', '1961', '1961', '2356', '6096', '3281', '2524', '3836', '3646', '4583', '4755', '3965', '3281', '3646', '3836', '3109', '3109', '465', '465', '3087', '1592', '2356', '1961', '1961', '2356', '6096', '3281', '2524', '3836', '3646', '4583', '4755', '3646', '3281', '3836', '3965', '3109', '3109', '465', '3087', '1592', '2356', '1961', '1961', '2356', '6096', '3281', '2524', '3836', '3646', '4583', '4755', '3965', '3281', '3646', '3836', '3109', '3109', '465', '2500', '3120', '2991', '3490', '3100');
INSERT INTO "hp" VALUES ('87', '470', '3153', '1623', '2405', '2001', '2001', '2405', '6232', '3351', '2577', '3919', '3725', '4684', '4858', '4049', '3351', '3725', '3919', '3177', '3177', '470', '470', '3153', '1623', '2405', '2001', '2001', '2405', '6232', '3351', '2577', '3919', '3725', '4684', '4858', '3725', '3351', '3919', '4049', '3177', '3177', '470', '3153', '1623', '2405', '2001', '2001', '2405', '6232', '3351', '2577', '3919', '3725', '4684', '4858', '4049', '3351', '3725', '3919', '3177', '3177', '470', '2550', '3140', '3011', '3565', '3170');
INSERT INTO "hp" VALUES ('88', '475', '3220', '1654', '2454', '2041', '2041', '2454', '6369', '3422', '2630', '4003', '3805', '4786', '4962', '4135', '3422', '3805', '4003', '3246', '3246', '475', '475', '3220', '1654', '2454', '2041', '2041', '2454', '6369', '3422', '2630', '4003', '3805', '4786', '4962', '3805', '3422', '4003', '4135', '3246', '3246', '475', '3220', '1654', '2454', '2041', '2041', '2454', '6369', '3422', '2630', '4003', '3805', '4786', '4962', '4135', '3422', '3805', '4003', '3246', '3246', '475', '2600', '3160', '3032', '3640', '3240');
INSERT INTO "hp" VALUES ('89', '480', '3287', '1686', '2504', '2082', '2082', '2504', '6508', '3494', '2684', '4088', '3886', '4889', '5067', '4221', '3494', '3886', '4088', '3316', '3316', '480', '480', '3287', '1686', '2504', '2082', '2082', '2504', '6508', '3494', '2684', '4088', '3886', '4889', '5067', '3886', '3494', '4088', '4221', '3316', '3316', '480', '3287', '1686', '2504', '2082', '2082', '2504', '6508', '3494', '2684', '4088', '3886', '4889', '5067', '4221', '3494', '3886', '4088', '3316', '3316', '480', '2650', '3180', '3054', '3715', '3310');
INSERT INTO "hp" VALUES ('90', '485', '3355', '1718', '2554', '2123', '2123', '2554', '6648', '3567', '2739', '4174', '3968', '4993', '5173', '4309', '3567', '3968', '4174', '3387', '3387', '485', '485', '3355', '1718', '2554', '2123', '2123', '2554', '6648', '3567', '2739', '4174', '3968', '4993', '5173', '3968', '3567', '4174', '4309', '3387', '3387', '485', '3355', '1718', '2554', '2123', '2123', '2554', '6648', '3567', '2739', '4174', '3968', '4993', '5173', '4309', '3567', '3968', '4174', '3387', '3387', '485', '2700', '3455', '3567', '3790', '3380');
INSERT INTO "hp" VALUES ('91', '490', '3424', '1750', '2605', '2164', '2164', '2605', '6790', '3640', '2794', '4261', '4050', '5098', '5280', '4397', '3640', '4050', '4261', '3458', '3458', '490', '490', '3424', '1750', '2605', '2164', '2164', '2605', '6790', '3640', '2794', '4261', '4050', '5098', '5280', '4050', '3640', '4261', '4397', '3458', '3458', '490', '3424', '1750', '2605', '2164', '2164', '2605', '6790', '3640', '2794', '4261', '4050', '5098', '5280', '4397', '3640', '4050', '4261', '3458', '3458', '490', '2750', '3524', '3590', '3870', '3455');
INSERT INTO "hp" VALUES ('92', '495', '3493', '1783', '2656', '2206', '2206', '2656', '6933', '3714', '2850', '4349', '4133', '5204', '5388', '4487', '3714', '4133', '4349', '3530', '3530', '495', '495', '3493', '1783', '2656', '2206', '2206', '2656', '6933', '3714', '2850', '4349', '4133', '5204', '5388', '4133', '3714', '4349', '4487', '3530', '3530', '495', '3493', '1783', '2656', '2206', '2206', '2656', '6933', '3714', '2850', '4349', '4133', '5204', '5388', '4487', '3714', '4133', '4349', '3530', '3530', '495', '2800', '3593', '3614', '3950', '3530');
INSERT INTO "hp" VALUES ('93', '500', '3563', '1816', '2708', '2248', '2248', '2708', '7078', '3789', '2906', '4438', '4217', '5311', '5497', '4577', '3789', '4217', '4438', '3603', '3603', '500', '500', '3563', '1816', '2708', '2248', '2248', '2708', '7078', '3789', '2906', '4438', '4217', '5311', '5497', '4217', '3789', '4438', '4577', '3603', '3603', '500', '3563', '1816', '2708', '2248', '2248', '2708', '7078', '3789', '2906', '4438', '4217', '5311', '5497', '4577', '3789', '4217', '4438', '3603', '3603', '500', '2850', '3663', '3649', '4030', '3605');
INSERT INTO "hp" VALUES ('94', '505', '3634', '1849', '2760', '2291', '2291', '2760', '7224', '3865', '2963', '4528', '4302', '5419', '5607', '4669', '3865', '4302', '4528', '3677', '3677', '505', '505', '3634', '1849', '2760', '2291', '2291', '2760', '7224', '3865', '2963', '4528', '4302', '5419', '5607', '4302', '3865', '4528', '4669', '3677', '3677', '505', '3634', '1849', '2760', '2291', '2291', '2760', '7224', '3865', '2963', '4528', '4302', '5419', '5607', '4669', '3865', '4302', '4528', '3677', '3677', '505', '2900', '3834', '3675', '4110', '3680');
INSERT INTO "hp" VALUES ('95', '510', '3706', '1883', '2813', '2334', '2334', '2813', '7372', '3941', '3020', '4619', '4388', '5529', '5719', '4761', '3941', '4388', '4619', '3751', '3751', '510', '510', '3706', '1883', '2813', '2334', '2334', '2813', '7372', '3941', '3020', '4619', '4388', '5529', '5719', '4388', '3941', '4619', '4761', '3751', '3751', '510', '3706', '1883', '2813', '2334', '2334', '2813', '7372', '3941', '3020', '4619', '4388', '5529', '5719', '4761', '3941', '4388', '4619', '3751', '3751', '510', '2950', '3806', '3701', '4190', '3760');
INSERT INTO "hp" VALUES ('96', '515', '3778', '1917', '2866', '2377', '2377', '2866', '7521', '4018', '3078', '4710', '4475', '5640', '5832', '4854', '4018', '4475', '4710', '3826', '3826', '515', '515', '3778', '1917', '2866', '2377', '2377', '2866', '7521', '4018', '3078', '4710', '4475', '5640', '5832', '4475', '4018', '4710', '4854', '3826', '3826', '515', '3778', '1917', '2866', '2377', '2377', '2866', '7521', '4018', '3078', '4710', '4475', '5640', '5832', '4854', '4018', '4475', '4710', '3826', '3826', '515', '3000', '3878', '3728', '4270', '3840');
INSERT INTO "hp" VALUES ('97', '520', '3851', '1951', '2920', '2421', '2421', '2920', '7672', '4096', '3136', '4802', '4562', '5752', '5946', '4947', '4096', '4562', '4802', '3902', '3902', '520', '520', '3851', '1951', '2920', '2421', '2421', '2920', '7672', '4096', '3136', '4802', '4562', '5752', '5946', '4562', '4096', '4802', '4947', '3902', '3902', '520', '3851', '1951', '2920', '2421', '2421', '2920', '7672', '4096', '3136', '4802', '4562', '5752', '5946', '4947', '4096', '4562', '4802', '3902', '3902', '520', '3050', '3951', '3756', '4350', '3920');
INSERT INTO "hp" VALUES ('98', '525', '3925', '1985', '2974', '2465', '2465', '2974', '7824', '4175', '3195', '4895', '4650', '5865', '6061', '5042', '4175', '4650', '4895', '3979', '3979', '525', '525', '3925', '1985', '2974', '2465', '2465', '2974', '7824', '4175', '3195', '4895', '4650', '5865', '6061', '4650', '4175', '4895', '5042', '3979', '3979', '525', '3925', '1985', '2974', '2465', '2465', '2974', '7824', '4175', '3195', '4895', '4650', '5865', '6061', '5042', '4175', '4650', '4895', '3979', '3979', '525', '3100', '4025', '3800', '4430', '4000');
INSERT INTO "hp" VALUES ('99', '530', '3999', '2020', '3029', '2510', '2510', '3029', '7978', '4254', '3254', '4989', '4739', '5979', '6177', '5137', '4254', '4739', '4989', '4056', '4056', '530', '530', '3999', '2020', '3029', '2510', '2510', '3029', '7978', '4254', '3254', '4989', '4739', '5979', '6177', '4739', '4254', '4989', '5137', '4056', '4056', '530', '3999', '2020', '3029', '2510', '2510', '3029', '7978', '4254', '3254', '4989', '4739', '5979', '6177', '5137', '4254', '4739', '4989', '4056', '4056', '3200', '4500', '4250', '4510', '4080', null);
INSERT INTO "maps" VALUES ('06guild_01', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0');
INSERT INTO "maps" VALUES ('06guild_02', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0');
INSERT INTO "maps" VALUES ('06guild_03', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0');
INSERT INTO "maps" VALUES ('06guild_04', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0');
INSERT INTO "maps" VALUES ('06guild_05', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0');
INSERT INTO "maps" VALUES ('06guild_06', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0');
INSERT INTO "maps" VALUES ('06guild_07', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0');
INSERT INTO "maps" VALUES ('06guild_08', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0');
INSERT INTO "maps" VALUES ('06guild_r', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('abyss_01', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('abyss_02', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('abyss_03', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('airplane', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('airplane_01', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('airport', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('alb2trea', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('alberta', '1', '1', '0', '1', '0', '0', '0', '0', '0', '1', '0', '0', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('alberta_in', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('alb_ship', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('aldebaran', '1', '1', '0', '1', '0', '0', '0', '0', '0', '1', '0', '0', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('aldeba_in', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('aldeg_cas01', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0');
INSERT INTO "maps" VALUES ('aldeg_cas02', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0');
INSERT INTO "maps" VALUES ('aldeg_cas03', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0');
INSERT INTO "maps" VALUES ('aldeg_cas04', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0');
INSERT INTO "maps" VALUES ('aldeg_cas05', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0');
INSERT INTO "maps" VALUES ('alde_alche', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('alde_dun01', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('alde_dun02', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('alde_dun03', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('alde_dun04', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('alde_gld', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('alde_tt02', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '0', '0', '0', '1', '0', '0', '0');
INSERT INTO "maps" VALUES ('amatsu', '1', '1', '0', '1', '0', '0', '0', '0', '0', '1', '0', '0', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ama_dun01', '1', '0', '0', '0', '0', '1', '0', '0', '0', '1', '0', '1', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ama_dun02', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ama_dun03', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ama_fild01', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ama_in01', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ama_in02', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ama_test', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('anthell01', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('anthell02', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('arena_room', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('auction_01', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('auction_02', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ayothaya', '1', '1', '0', '1', '0', '0', '0', '0', '0', '1', '0', '0', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ayo_dun01', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ayo_dun02', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ayo_fild01', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ayo_fild02', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ayo_in01', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ayo_in02', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('beach_dun', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('beach_dun2', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('beach_dun3', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('cmd_fild01', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('cmd_fild02', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('cmd_fild03', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('cmd_fild04', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('cmd_fild05', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('cmd_fild06', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('cmd_fild07', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('cmd_fild08', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('cmd_fild09', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('cmd_in01', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('cmd_in02', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('comodo', '1', '1', '0', '1', '0', '0', '0', '0', '0', '1', '0', '0', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('c_tower1', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('c_tower2', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('c_tower3', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('c_tower4', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('einbech', '1', '1', '0', '1', '0', '0', '0', '0', '0', '1', '0', '0', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('einbroch', '1', '1', '0', '1', '0', '0', '0', '0', '0', '1', '0', '0', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ein_dun01', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ein_dun02', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ein_fild01', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ein_fild02', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ein_fild03', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ein_fild04', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ein_fild05', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ein_fild06', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ein_fild07', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ein_fild08', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ein_fild09', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ein_fild10', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ein_in01', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('force_1-1', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('force_1-2', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('force_1-3', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('force_2-1', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('force_2-2', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('force_2-3', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('force_3-1', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('force_3-2', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('force_3-3', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('force_4-1', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('force_5-1', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('force_map1', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('force_map2', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('force_map3', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('gefenia01', '1', '0', '0', '0', '0', '1', '0', '0', '0', '1', '0', '1', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('gefenia02', '1', '0', '0', '0', '0', '1', '0', '0', '0', '1', '0', '1', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('gefenia03', '1', '0', '0', '0', '0', '1', '0', '0', '0', '1', '0', '1', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('gefenia04', '1', '0', '0', '0', '0', '1', '0', '0', '0', '1', '0', '1', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('geffen', '1', '1', '0', '1', '0', '0', '0', '0', '0', '1', '0', '0', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('geffen_in', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('gefg_cas01', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0');
INSERT INTO "maps" VALUES ('gefg_cas02', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0');
INSERT INTO "maps" VALUES ('gefg_cas03', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0');
INSERT INTO "maps" VALUES ('gefg_cas04', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0');
INSERT INTO "maps" VALUES ('gefg_cas05', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0');
INSERT INTO "maps" VALUES ('gef_dun00', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('gef_dun01', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('gef_dun02', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('gef_dun03', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('gef_fild00', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('gef_fild01', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('gef_fild02', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('gef_fild03', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('gef_fild04', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('gef_fild05', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('gef_fild06', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('gef_fild07', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('gef_fild08', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('gef_fild09', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('gef_fild10', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('gef_fild11', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('gef_fild12', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('gef_fild13', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('gef_fild14', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('gef_tower', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('glast_01', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('gld_dun01', '1', '0', '1', '1', '0', '1', '1', '0', '0', '1', '0', '1', '1', '1', '0', '0', '1', '0');
INSERT INTO "maps" VALUES ('gld_dun02', '1', '0', '1', '1', '0', '1', '1', '0', '0', '1', '0', '1', '1', '1', '0', '0', '1', '0');
INSERT INTO "maps" VALUES ('gld_dun03', '1', '0', '1', '1', '0', '1', '1', '0', '0', '1', '0', '1', '1', '1', '0', '0', '1', '0');
INSERT INTO "maps" VALUES ('gld_dun04', '1', '0', '1', '1', '0', '1', '1', '0', '0', '1', '0', '1', '1', '1', '0', '0', '1', '0');
INSERT INTO "maps" VALUES ('gl_cas01', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('gl_cas02', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('gl_church', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('gl_chyard', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('gl_dun01', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('gl_dun02', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('gl_in01', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('gl_knt01', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('gl_knt02', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('gl_prison', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('gl_prison1', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('gl_sew01', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('gl_sew02', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('gl_sew03', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('gl_sew04', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('gl_step', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('gonryun', '1', '1', '0', '1', '0', '0', '0', '0', '0', '1', '0', '0', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('gon_dun01', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('gon_dun02', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('gon_dun03', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('gon_fild01', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('gon_in', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('gon_test', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('guild_room', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0');
INSERT INTO "maps" VALUES ('guild_vs1', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('guild_vs1-1', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0');
INSERT INTO "maps" VALUES ('guild_vs1-2', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0');
INSERT INTO "maps" VALUES ('guild_vs1-3', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0');
INSERT INTO "maps" VALUES ('guild_vs1-4', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0');
INSERT INTO "maps" VALUES ('guild_vs2', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('guild_vs2-1', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0');
INSERT INTO "maps" VALUES ('guild_vs2-2', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0');
INSERT INTO "maps" VALUES ('guild_vs3', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0');
INSERT INTO "maps" VALUES ('guild_vs4', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0');
INSERT INTO "maps" VALUES ('guild_vs5', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0');
INSERT INTO "maps" VALUES ('g_room1-1', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('g_room1-2', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('g_room1-3', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('g_room2', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('himinn', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('hugel', '1', '1', '0', '1', '0', '0', '0', '0', '0', '1', '0', '0', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('hunter_1-1', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('hunter_2-1', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('hunter_3-1', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('hu_fild01', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('hu_fild02', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('hu_fild03', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('hu_fild04', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('hu_fild05', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('hu_fild06', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('hu_fild07', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('hu_in01', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ice_dun01', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ice_dun02', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ice_dun03', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ice_dun04', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('in_moc_16', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('in_orcs01', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('in_rogue', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('in_sphinx1', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('in_sphinx2', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('in_sphinx3', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('in_sphinx4', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('in_sphinx5', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('izlu2dun', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('izlude', '1', '1', '0', '1', '0', '0', '0', '0', '0', '1', '0', '0', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('izlude_in', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('iz_dun00', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('iz_dun01', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('iz_dun02', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('iz_dun03', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('iz_dun04', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('jawaii', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('jawaii_in', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('job_cru', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('job_duncer', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('job_hunte', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('job_hunter', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('job_knight', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('job_knt', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('job_monk', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('job_priest', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('job_prist', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('job_sage', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('job_soul', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('job_star', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('job_sword1', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('job_thief1', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('job_wiz', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('job_wizard', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('juperos_01', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('juperos_02', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('jupe_area1', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('jupe_area2', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('jupe_cave', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('jupe_core', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('jupe_ele', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('jupe_ele_r', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('jupe_gate', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('kh_dun01', '1', '0', '0', '0', '0', '1', '0', '0', '0', '1', '0', '1', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('kh_dun02', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('kh_kiehl01', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('kh_kiehl02', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('kh_mansion', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('kh_rossi', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('kh_school', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('kh_vila', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('knight_1-1', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('knight_2-1', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('knight_3-1', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('lhz_airport', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('lhz_cube', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('lhz_dun01', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('lhz_dun02', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('lhz_dun03', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('lhz_fild01', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('lhz_fild02', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('lhz_fild03', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('lhz_in01', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('lhz_in02', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('lhz_in03', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('lhz_que01', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('lighthalzen', '1', '1', '0', '1', '0', '0', '0', '0', '0', '1', '0', '0', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('louyang', '1', '1', '0', '1', '0', '0', '0', '0', '0', '1', '0', '0', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('lou_dun01', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('lou_dun02', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('lou_dun03', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('lou_fild01', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('lou_in01', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('lou_in02', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('mag_dun01', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('mag_dun02', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('mapname', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('mjolnir_01', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('mjolnir_02', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('mjolnir_03', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('mjolnir_04', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('mjolnir_05', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('mjolnir_06', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('mjolnir_07', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('mjolnir_08', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('mjolnir_09', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('mjolnir_10', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('mjolnir_11', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('mjolnir_12', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('mjo_dun01', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('mjo_dun02', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('mjo_dun03', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('moc_castle', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('moc_fild01', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('moc_fild02', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('moc_fild03', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('moc_fild04', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('moc_fild05', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('moc_fild06', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('moc_fild07', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('moc_fild08', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('moc_fild09', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('moc_fild10', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('moc_fild11', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('moc_fild12', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('moc_fild13', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('moc_fild14', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('moc_fild15', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('moc_fild16', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('moc_fild17', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('moc_fild18', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('moc_fild19', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('moc_pryd01', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('moc_pryd02', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('moc_pryd03', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('moc_pryd04', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('moc_pryd05', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('moc_pryd06', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('moc_prydb1', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('moc_ruins', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('monk_in', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('monk_test', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('morocc', '1', '1', '0', '1', '0', '0', '0', '0', '0', '1', '0', '0', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('morocc_in', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('new_1-1', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('new_1-2', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('new_1-3', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('new_1-4', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('new_2-1', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('new_2-2', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('new_2-3', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('new_2-4', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('new_3-1', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('new_3-2', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('new_3-3', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('new_3-4', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('new_4-1', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('new_4-2', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('new_4-3', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('new_4-4', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('new_5-1', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('new_5-2', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('new_5-3', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('new_5-4', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('new_zone01', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '1', '0', '0', '0');
INSERT INTO "maps" VALUES ('new_zone02', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '1', '0', '0', '0');
INSERT INTO "maps" VALUES ('new_zone03', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '1', '0', '0', '0');
INSERT INTO "maps" VALUES ('new_zone04', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '1', '0', '0', '0');
INSERT INTO "maps" VALUES ('nguild_alde', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0');
INSERT INTO "maps" VALUES ('nguild_gef', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0');
INSERT INTO "maps" VALUES ('nguild_pay', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0');
INSERT INTO "maps" VALUES ('nguild_prt', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0');
INSERT INTO "maps" VALUES ('niflheim', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('nif_fild01', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('nif_fild02', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('nif_in', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('n_castle', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('odin_tem01', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('odin_tem02', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('odin_tem03', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('orcsdun01', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('orcsdun02', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ordeal_1-1', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ordeal_1-2', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ordeal_1-3', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ordeal_1-4', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ordeal_2-1', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ordeal_2-2', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ordeal_2-3', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ordeal_2-4', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ordeal_3-1', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ordeal_3-2', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ordeal_3-3', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ordeal_3-4', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ordeal_a00', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ordeal_a02', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('payg_cas01', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0');
INSERT INTO "maps" VALUES ('payg_cas02', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0');
INSERT INTO "maps" VALUES ('payg_cas03', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0');
INSERT INTO "maps" VALUES ('payg_cas04', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0');
INSERT INTO "maps" VALUES ('payg_cas05', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0');
INSERT INTO "maps" VALUES ('payon', '1', '1', '0', '1', '0', '0', '0', '0', '0', '1', '0', '0', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('payon_in01', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('payon_in02', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('payon_in03', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('pay_arche', '1', '1', '0', '1', '0', '0', '0', '0', '0', '1', '0', '0', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('pay_dun00', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('pay_dun01', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('pay_dun02', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('pay_dun03', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('pay_dun04', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('pay_fild01', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('pay_fild02', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('pay_fild03', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('pay_fild04', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('pay_fild05', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('pay_fild06', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('pay_fild07', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('pay_fild08', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('pay_fild09', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('pay_fild10', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('pay_fild11', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('pay_gld', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('poring_c01', '1', '0', '1', '0', '0', '1', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('poring_c02', '1', '0', '1', '0', '0', '1', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('priest_1-1', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('priest_2-1', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('priest_3-1', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('prontera', '1', '1', '0', '1', '0', '0', '0', '0', '0', '1', '0', '0', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('prtg_cas01', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0');
INSERT INTO "maps" VALUES ('prtg_cas02', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0');
INSERT INTO "maps" VALUES ('prtg_cas03', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0');
INSERT INTO "maps" VALUES ('prtg_cas04', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0');
INSERT INTO "maps" VALUES ('prtg_cas05', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0');
INSERT INTO "maps" VALUES ('prt_are01', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('prt_are_in', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('prt_castle', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('prt_church', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('prt_fild00', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('prt_fild01', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('prt_fild02', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('prt_fild03', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('prt_fild04', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('prt_fild05', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('prt_fild06', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('prt_fild07', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('prt_fild08', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('prt_fild09', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('prt_fild10', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('prt_fild11', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('prt_gld', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('prt_in', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('prt_maze01', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('prt_maze02', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('prt_maze03', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('prt_monk', '1', '1', '0', '1', '0', '0', '0', '0', '0', '1', '0', '0', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('prt_sewb1', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('prt_sewb2', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('prt_sewb3', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('prt_sewb4', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('pvp_2vs2', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_c_room', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('pvp_n_1-1', '1', '0', '1', '1', '1', '1', '1', '0', '0', '1', '0', '1', '1', '1', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_n_1-2', '1', '0', '1', '1', '1', '1', '1', '0', '0', '1', '0', '1', '1', '1', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_n_1-3', '1', '0', '1', '1', '1', '1', '1', '0', '0', '1', '0', '1', '1', '1', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_n_1-4', '1', '0', '1', '1', '1', '1', '1', '0', '0', '1', '0', '1', '1', '1', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_n_1-5', '1', '0', '1', '1', '1', '1', '1', '0', '0', '1', '0', '1', '1', '1', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_n_2-1', '1', '0', '1', '1', '1', '1', '1', '0', '0', '1', '0', '1', '1', '1', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_n_2-2', '1', '0', '1', '1', '1', '1', '1', '0', '0', '1', '0', '1', '1', '1', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_n_2-3', '1', '0', '1', '1', '1', '1', '1', '0', '0', '1', '0', '1', '1', '1', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_n_2-4', '1', '0', '1', '1', '1', '1', '1', '0', '0', '1', '0', '1', '1', '1', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_n_2-5', '1', '0', '1', '1', '1', '1', '1', '0', '0', '1', '0', '1', '1', '1', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_n_3-1', '1', '0', '1', '1', '1', '1', '1', '0', '0', '1', '0', '1', '1', '1', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_n_3-2', '1', '0', '1', '1', '1', '1', '1', '0', '0', '1', '0', '1', '1', '1', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_n_3-3', '1', '0', '1', '1', '1', '1', '1', '0', '0', '1', '0', '1', '1', '1', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_n_3-4', '1', '0', '1', '1', '1', '1', '1', '0', '0', '1', '0', '1', '1', '1', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_n_3-5', '1', '0', '1', '1', '1', '1', '1', '0', '0', '1', '0', '1', '1', '1', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_n_4-1', '1', '0', '1', '1', '1', '1', '1', '0', '0', '1', '0', '1', '1', '1', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_n_4-2', '1', '0', '1', '1', '1', '1', '1', '0', '0', '1', '0', '1', '1', '1', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_n_4-3', '1', '0', '1', '1', '1', '1', '1', '0', '0', '1', '0', '1', '1', '1', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_n_4-4', '1', '0', '1', '1', '1', '1', '1', '0', '0', '1', '0', '1', '1', '1', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_n_4-5', '1', '0', '1', '1', '1', '1', '1', '0', '0', '1', '0', '1', '1', '1', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_n_5-1', '1', '0', '1', '1', '1', '1', '1', '0', '0', '1', '0', '1', '1', '1', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_n_5-2', '1', '0', '1', '1', '1', '1', '1', '0', '0', '1', '0', '1', '1', '1', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_n_5-3', '1', '0', '1', '1', '1', '1', '1', '0', '0', '1', '0', '1', '1', '1', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_n_5-4', '1', '0', '1', '1', '1', '1', '1', '0', '0', '1', '0', '1', '1', '1', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_n_5-5', '1', '0', '1', '1', '1', '1', '1', '0', '0', '1', '0', '1', '1', '1', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_n_6-1', '1', '0', '1', '1', '1', '1', '1', '0', '0', '1', '0', '1', '1', '1', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_n_6-2', '1', '0', '1', '1', '1', '1', '1', '0', '0', '1', '0', '1', '1', '1', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_n_6-3', '1', '0', '1', '1', '1', '1', '1', '0', '0', '1', '0', '1', '1', '1', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_n_6-4', '1', '0', '1', '1', '1', '1', '1', '0', '0', '1', '0', '1', '1', '1', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_n_6-5', '1', '0', '1', '1', '1', '1', '1', '0', '0', '1', '0', '1', '1', '1', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_n_7-1', '1', '0', '1', '1', '1', '1', '1', '0', '0', '1', '0', '1', '1', '1', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_n_7-2', '1', '0', '1', '1', '1', '1', '1', '0', '0', '1', '0', '1', '1', '1', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_n_7-3', '1', '0', '1', '1', '1', '1', '1', '0', '0', '1', '0', '1', '1', '1', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_n_7-4', '1', '0', '1', '1', '1', '1', '1', '0', '0', '1', '0', '1', '1', '1', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_n_7-5', '1', '0', '1', '1', '1', '1', '1', '0', '0', '1', '0', '1', '1', '1', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_n_8-1', '1', '0', '1', '1', '1', '1', '1', '0', '0', '1', '0', '1', '1', '1', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_n_8-2', '1', '0', '1', '1', '1', '1', '1', '0', '0', '1', '0', '1', '1', '1', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_n_8-3', '1', '0', '1', '1', '1', '1', '1', '0', '0', '1', '0', '1', '1', '1', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_n_8-4', '1', '0', '1', '1', '1', '1', '1', '0', '0', '1', '0', '1', '1', '1', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_n_8-5', '1', '0', '1', '1', '1', '1', '1', '0', '0', '1', '0', '1', '1', '1', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_n_room', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('pvp_room', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('pvp_y_1-1', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_y_1-2', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_y_1-3', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_y_1-4', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_y_1-5', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_y_2-1', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_y_2-2', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_y_2-3', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_y_2-4', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_y_2-5', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_y_3-1', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_y_3-2', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_y_3-3', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_y_3-4', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_y_3-5', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_y_4-1', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_y_4-2', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_y_4-3', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_y_4-4', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_y_4-5', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_y_5-1', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_y_5-2', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_y_5-3', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_y_5-4', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_y_5-5', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_y_6-1', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_y_6-2', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_y_6-3', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_y_6-4', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_y_6-5', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_y_7-1', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_y_7-2', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_y_7-3', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_y_7-4', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_y_7-5', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_y_8-1', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_y_8-2', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_y_8-3', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_y_8-4', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_y_8-5', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '0');
INSERT INTO "maps" VALUES ('pvp_y_room', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('p_track01', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('p_track02', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('que_bingo', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('que_god01', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('que_god02', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('que_hugel', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '1', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('que_job01', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('que_job02', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('que_job03', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('que_ng', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('que_rachel', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('que_san04', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('que_sign01', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('que_sign02', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('que_thor', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('quiz_00', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('quiz_01', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('quiz_02', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('quiz_test', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('rachel', '1', '1', '0', '1', '0', '0', '0', '0', '0', '1', '0', '0', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ra_fild01', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ra_fild02', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ra_fild03', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ra_fild04', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ra_fild05', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ra_fild06', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ra_fild07', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ra_fild08', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ra_fild09', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ra_fild10', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ra_fild11', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ra_fild12', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ra_fild13', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ra_in01', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ra_san01', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ra_san02', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ra_san03', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ra_san04', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ra_san05', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ra_temin', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ra_temple', '1', '1', '0', '1', '0', '0', '0', '0', '0', '1', '0', '0', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ra_temsky', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('sec_in01', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('sec_in02', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('sec_pri', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '1', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('siege_test', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('sword_1-1', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('sword_2-1', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('sword_3-1', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('thana_boss', '1', '0', '1', '0', '0', '1', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('thana_step', '1', '0', '0', '0', '0', '1', '0', '0', '0', '1', '0', '1', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('tha_scene01', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('tha_t01', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('tha_t02', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('tha_t03', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('tha_t04', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('tha_t05', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('tha_t06', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('tha_t07', '1', '0', '0', '0', '0', '1', '0', '0', '0', '1', '0', '1', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('tha_t08', '1', '0', '0', '0', '0', '1', '0', '0', '0', '1', '0', '1', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('tha_t09', '1', '0', '0', '0', '0', '1', '0', '0', '0', '1', '0', '1', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('tha_t10', '1', '0', '0', '0', '0', '1', '0', '0', '0', '1', '0', '1', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('tha_t11', '1', '0', '0', '0', '0', '1', '0', '0', '0', '1', '0', '1', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('tha_t12', '1', '0', '0', '0', '0', '1', '0', '0', '0', '1', '0', '1', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('thor_camp', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('thor_v01', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('thor_v02', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('thor_v03', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('treasure01', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('treasure02', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('turbo_e_16', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '0', '0', '0', '1', '0', '0', '0');
INSERT INTO "maps" VALUES ('turbo_e_4', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '0', '0', '0', '1', '0', '0', '0');
INSERT INTO "maps" VALUES ('turbo_e_8', '1', '0', '1', '0', '0', '0', '1', '0', '0', '1', '0', '0', '0', '0', '1', '0', '0', '0');
INSERT INTO "maps" VALUES ('turbo_n_1', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '1', '0', '0', '0');
INSERT INTO "maps" VALUES ('turbo_n_16', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '1', '0', '0', '0');
INSERT INTO "maps" VALUES ('turbo_n_4', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '1', '0', '0', '0');
INSERT INTO "maps" VALUES ('turbo_n_8', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '1', '0', '0', '0');
INSERT INTO "maps" VALUES ('turbo_room', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('tur_dun01', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('tur_dun02', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('tur_dun03', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('tur_dun04', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('tur_dun05', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('tur_dun06', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('umbala', '1', '1', '0', '1', '0', '0', '0', '0', '0', '1', '0', '0', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('um_dun01', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('um_dun02', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('um_fild01', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('um_fild02', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('um_fild03', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('um_fild04', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('um_in', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('valkyrie', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('veins', '1', '1', '0', '1', '0', '0', '0', '0', '0', '1', '0', '0', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ve_fild01', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ve_fild02', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ve_fild03', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ve_fild04', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ve_fild05', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ve_fild06', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ve_fild07', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ve_in', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('ve_in02', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('wizard_1-1', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('wizard_2-1', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('wizard_3-1', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('xmas', '1', '1', '0', '1', '0', '0', '0', '0', '0', '1', '0', '0', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('xmas_dun01', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('xmas_dun02', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('xmas_fild01', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('xmas_in', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('yggdrasil01', '1', '0', '1', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('yuno', '1', '1', '0', '1', '0', '0', '0', '0', '0', '1', '0', '0', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('yuno_fild01', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('yuno_fild02', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('yuno_fild03', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('yuno_fild04', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('yuno_fild05', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('yuno_fild06', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('yuno_fild07', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('yuno_fild08', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('yuno_fild09', '1', '0', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('yuno_fild10', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('yuno_fild11', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('yuno_fild12', '1', '1', '0', '1', '0', '1', '0', '0', '0', '1', '0', '1', '1', '1', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('yuno_in01', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('yuno_in02', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('yuno_in03', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('yuno_in04', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('yuno_in05', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('yuno_pre', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "maps" VALUES ('y_airport', '1', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO "sp" VALUES (null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null);
INSERT INTO "sp" VALUES ('1', '11', '12', '16', '12', '15', '13', '12', '13', '18', '19', '14', '14', '14', '14', '14', '17', '15', '14', '15', '15', '11', '11', '12', '16', '12', '15', '13', '12', '13', '18', '19', '14', '14', '14', '14', '15', '17', '14', '14', '15', '15', '11', '12', '16', '12', '15', '13', '12', '13', '18', '19', '14', '14', '14', '14', '14', '17', '15', '14', '15', '15', '11', '12', '14', '19', '12', '14');
INSERT INTO "sp" VALUES ('2', '12', '14', '22', '14', '20', '16', '14', '16', '26', '28', '18', '18', '18', '19', '19', '24', '20', '18', '22', '22', '12', '12', '14', '22', '14', '20', '16', '14', '16', '26', '28', '18', '18', '18', '19', '20', '24', '18', '19', '22', '22', '12', '14', '22', '14', '20', '16', '14', '16', '26', '28', '18', '18', '18', '19', '19', '24', '20', '18', '22', '22', '12', '14', '19', '28', '15', '17');
INSERT INTO "sp" VALUES ('3', '13', '16', '28', '16', '25', '19', '16', '19', '34', '37', '22', '22', '22', '24', '24', '31', '25', '22', '28', '28', '13', '13', '16', '28', '16', '25', '19', '16', '19', '34', '37', '22', '22', '22', '24', '25', '31', '22', '24', '28', '28', '13', '16', '28', '16', '25', '19', '16', '19', '34', '37', '22', '22', '22', '24', '24', '31', '25', '22', '28', '28', '13', '16', '24', '37', '18', '20');
INSERT INTO "sp" VALUES ('4', '14', '18', '34', '18', '30', '22', '18', '22', '42', '46', '26', '26', '26', '28', '28', '38', '30', '26', '34', '34', '14', '14', '18', '34', '18', '30', '22', '18', '22', '42', '46', '26', '26', '26', '28', '30', '38', '26', '28', '34', '34', '14', '18', '34', '18', '30', '22', '18', '22', '42', '46', '26', '26', '26', '28', '28', '38', '30', '26', '34', '34', '14', '18', '28', '46', '21', '23');
INSERT INTO "sp" VALUES ('5', '15', '20', '40', '20', '35', '25', '20', '25', '50', '55', '30', '30', '30', '33', '33', '45', '35', '30', '40', '40', '15', '15', '20', '40', '20', '35', '25', '20', '25', '50', '55', '30', '30', '30', '33', '35', '45', '30', '33', '40', '40', '15', '20', '40', '20', '35', '25', '20', '25', '50', '55', '30', '30', '30', '33', '33', '45', '35', '30', '40', '40', '15', '20', '33', '55', '24', '26');
INSERT INTO "sp" VALUES ('6', '16', '22', '46', '22', '40', '28', '22', '28', '58', '64', '34', '34', '34', '38', '38', '52', '40', '34', '46', '46', '16', '16', '22', '46', '22', '40', '28', '22', '28', '58', '64', '34', '34', '34', '38', '40', '52', '34', '38', '46', '46', '16', '22', '46', '22', '40', '28', '22', '28', '58', '64', '34', '34', '34', '38', '38', '52', '40', '34', '46', '46', '16', '22', '38', '64', '27', '29');
INSERT INTO "sp" VALUES ('7', '17', '24', '52', '24', '45', '31', '24', '31', '66', '73', '38', '38', '38', '42', '42', '59', '45', '38', '52', '52', '17', '17', '24', '52', '24', '45', '31', '24', '31', '66', '73', '38', '38', '38', '42', '45', '59', '38', '42', '52', '52', '17', '24', '52', '24', '45', '31', '24', '31', '66', '73', '38', '38', '38', '42', '42', '59', '45', '38', '52', '52', '17', '24', '42', '73', '30', '32');
INSERT INTO "sp" VALUES ('8', '18', '26', '58', '26', '50', '34', '26', '34', '74', '82', '42', '42', '42', '47', '47', '66', '50', '42', '58', '58', '18', '18', '26', '58', '26', '50', '34', '26', '34', '74', '82', '42', '42', '42', '47', '50', '66', '42', '47', '58', '58', '18', '26', '58', '26', '50', '34', '26', '34', '74', '82', '42', '42', '42', '47', '47', '66', '50', '42', '58', '58', '18', '26', '47', '82', '33', '35');
INSERT INTO "sp" VALUES ('9', '19', '28', '64', '28', '55', '37', '28', '37', '82', '91', '46', '46', '46', '52', '52', '73', '55', '46', '64', '64', '19', '19', '28', '64', '28', '55', '37', '28', '37', '82', '91', '46', '46', '46', '52', '55', '73', '46', '52', '64', '64', '19', '28', '64', '28', '55', '37', '28', '37', '82', '91', '46', '46', '46', '52', '52', '73', '55', '46', '64', '64', '19', '28', '52', '91', '36', '38');
INSERT INTO "sp" VALUES ('10', '20', '30', '70', '30', '60', '40', '30', '40', '90', '100', '50', '50', '50', '57', '57', '80', '60', '50', '70', '70', '20', '20', '30', '70', '30', '60', '40', '30', '40', '90', '100', '50', '50', '50', '57', '60', '80', '50', '57', '70', '70', '20', '30', '70', '30', '60', '40', '30', '40', '90', '100', '50', '50', '50', '57', '57', '80', '60', '50', '70', '70', '20', '30', '57', '100', '39', '41');
INSERT INTO "sp" VALUES ('11', '21', '32', '76', '32', '65', '43', '32', '43', '98', '109', '54', '54', '54', '61', '61', '87', '65', '54', '76', '76', '21', '21', '32', '76', '32', '65', '43', '32', '43', '98', '109', '54', '54', '54', '61', '65', '87', '54', '61', '76', '76', '21', '32', '76', '32', '65', '43', '32', '43', '98', '109', '54', '54', '54', '61', '61', '87', '65', '54', '76', '76', '21', '32', '61', '109', '42', '44');
INSERT INTO "sp" VALUES ('12', '22', '34', '82', '34', '70', '46', '34', '46', '106', '118', '58', '58', '58', '66', '66', '94', '70', '58', '82', '82', '22', '22', '34', '82', '34', '70', '46', '34', '46', '106', '118', '58', '58', '58', '66', '70', '94', '58', '66', '82', '82', '22', '34', '82', '34', '70', '46', '34', '46', '106', '118', '58', '58', '58', '66', '66', '94', '70', '58', '82', '82', '22', '34', '66', '118', '45', '47');
INSERT INTO "sp" VALUES ('13', '23', '36', '88', '36', '75', '49', '36', '49', '114', '127', '62', '62', '62', '71', '71', '101', '75', '62', '88', '88', '23', '23', '36', '88', '36', '75', '49', '36', '49', '114', '127', '62', '62', '62', '71', '75', '101', '62', '71', '88', '88', '23', '36', '88', '36', '75', '49', '36', '49', '114', '127', '62', '62', '62', '71', '71', '101', '75', '62', '88', '88', '23', '36', '71', '127', '48', '50');
INSERT INTO "sp" VALUES ('14', '24', '38', '94', '38', '80', '52', '38', '52', '122', '136', '66', '66', '66', '75', '75', '108', '80', '66', '94', '94', '24', '24', '38', '94', '38', '80', '52', '38', '52', '122', '136', '66', '66', '66', '75', '80', '108', '66', '75', '94', '94', '24', '38', '94', '38', '80', '52', '38', '52', '122', '136', '66', '66', '66', '75', '75', '108', '80', '66', '94', '94', '24', '38', '75', '136', '51', '53');
INSERT INTO "sp" VALUES ('15', '25', '40', '100', '40', '85', '55', '40', '55', '130', '145', '70', '70', '70', '80', '80', '115', '85', '70', '100', '100', '25', '25', '40', '100', '40', '85', '55', '40', '55', '130', '145', '70', '70', '70', '80', '85', '115', '70', '80', '100', '100', '25', '40', '100', '40', '85', '55', '40', '55', '130', '145', '70', '70', '70', '80', '80', '115', '85', '70', '100', '100', '25', '40', '80', '145', '54', '56');
INSERT INTO "sp" VALUES ('16', '26', '42', '106', '42', '90', '58', '42', '58', '138', '154', '74', '74', '74', '85', '85', '122', '90', '74', '106', '106', '26', '26', '42', '106', '42', '90', '58', '42', '58', '138', '154', '74', '74', '74', '85', '90', '122', '74', '85', '106', '106', '26', '42', '106', '42', '90', '58', '42', '58', '138', '154', '74', '74', '74', '85', '85', '122', '90', '74', '106', '106', '26', '42', '85', '154', '57', '59');
INSERT INTO "sp" VALUES ('17', '27', '44', '112', '44', '95', '61', '44', '61', '146', '163', '78', '78', '78', '89', '89', '129', '95', '78', '112', '112', '27', '27', '44', '112', '44', '95', '61', '44', '61', '146', '163', '78', '78', '78', '89', '95', '129', '78', '89', '112', '112', '27', '44', '112', '44', '95', '61', '44', '61', '146', '163', '78', '78', '78', '89', '89', '129', '95', '78', '112', '112', '27', '44', '89', '163', '61', '62');
INSERT INTO "sp" VALUES ('18', '28', '46', '118', '46', '100', '64', '46', '64', '154', '172', '82', '82', '82', '94', '94', '136', '100', '82', '118', '118', '28', '28', '46', '118', '46', '100', '64', '46', '64', '154', '172', '82', '82', '82', '94', '100', '136', '82', '94', '118', '118', '28', '46', '118', '46', '100', '64', '46', '64', '154', '172', '82', '82', '82', '94', '94', '136', '100', '82', '118', '118', '28', '46', '94', '172', '64', '65');
INSERT INTO "sp" VALUES ('19', '29', '48', '124', '48', '105', '67', '48', '67', '162', '181', '86', '86', '86', '99', '99', '143', '105', '86', '124', '124', '29', '29', '48', '124', '48', '105', '67', '48', '67', '162', '181', '86', '86', '86', '99', '105', '143', '86', '99', '124', '124', '29', '48', '124', '48', '105', '67', '48', '67', '162', '181', '86', '86', '86', '99', '99', '143', '105', '86', '124', '124', '29', '48', '99', '181', '67', '68');
INSERT INTO "sp" VALUES ('20', '30', '50', '130', '50', '110', '70', '50', '70', '170', '190', '90', '90', '90', '104', '104', '150', '110', '90', '130', '130', '30', '30', '50', '130', '50', '110', '70', '50', '70', '170', '190', '90', '90', '90', '104', '110', '150', '90', '104', '130', '130', '30', '50', '130', '50', '110', '70', '50', '70', '170', '190', '90', '90', '90', '104', '104', '150', '110', '90', '130', '130', '30', '50', '104', '190', '70', '71');
INSERT INTO "sp" VALUES ('21', '31', '52', '136', '52', '115', '73', '52', '73', '178', '199', '94', '94', '94', '108', '108', '157', '115', '94', '136', '136', '31', '31', '52', '136', '52', '115', '73', '52', '73', '178', '199', '94', '94', '94', '108', '115', '157', '94', '108', '136', '136', '31', '52', '136', '52', '115', '73', '52', '73', '178', '199', '94', '94', '94', '108', '108', '157', '115', '94', '136', '136', '31', '52', '108', '199', '73', '75');
INSERT INTO "sp" VALUES ('22', '32', '54', '142', '54', '120', '76', '54', '76', '186', '208', '98', '98', '98', '113', '113', '164', '120', '98', '142', '142', '32', '32', '54', '142', '54', '120', '76', '54', '76', '186', '208', '98', '98', '98', '113', '120', '164', '98', '113', '142', '142', '32', '54', '142', '54', '120', '76', '54', '76', '186', '208', '98', '98', '98', '113', '113', '164', '120', '98', '142', '142', '32', '54', '113', '208', '76', '79');
INSERT INTO "sp" VALUES ('23', '33', '56', '148', '56', '125', '79', '56', '79', '194', '217', '102', '102', '102', '118', '118', '171', '125', '102', '148', '148', '33', '33', '56', '148', '56', '125', '79', '56', '79', '194', '217', '102', '102', '102', '118', '125', '171', '102', '118', '148', '148', '33', '56', '148', '56', '125', '79', '56', '79', '194', '217', '102', '102', '102', '118', '118', '171', '125', '102', '148', '148', '33', '56', '118', '217', '79', '83');
INSERT INTO "sp" VALUES ('24', '34', '58', '154', '58', '130', '82', '58', '82', '202', '226', '106', '106', '106', '122', '122', '178', '130', '106', '154', '154', '34', '34', '58', '154', '58', '130', '82', '58', '82', '202', '226', '106', '106', '106', '122', '130', '178', '106', '122', '154', '154', '34', '58', '154', '58', '130', '82', '58', '82', '202', '226', '106', '106', '106', '122', '122', '178', '130', '106', '154', '154', '34', '58', '122', '226', '82', '87');
INSERT INTO "sp" VALUES ('25', '35', '60', '160', '60', '135', '85', '60', '85', '210', '235', '110', '110', '110', '127', '127', '185', '135', '110', '160', '160', '35', '35', '60', '160', '60', '135', '85', '60', '85', '210', '235', '110', '110', '110', '127', '135', '185', '110', '127', '160', '160', '35', '60', '160', '60', '135', '85', '60', '85', '210', '235', '110', '110', '110', '127', '127', '185', '135', '110', '160', '160', '35', '60', '127', '235', '85', '91');
INSERT INTO "sp" VALUES ('26', '36', '62', '166', '62', '140', '88', '62', '88', '218', '244', '114', '114', '114', '132', '132', '192', '140', '114', '166', '166', '36', '36', '62', '166', '62', '140', '88', '62', '88', '218', '244', '114', '114', '114', '132', '140', '192', '114', '132', '166', '166', '36', '62', '166', '62', '140', '88', '62', '88', '218', '244', '114', '114', '114', '132', '132', '192', '140', '114', '166', '166', '36', '62', '132', '244', '89', '95');
INSERT INTO "sp" VALUES ('27', '37', '64', '172', '64', '145', '91', '64', '91', '226', '253', '118', '118', '118', '136', '136', '199', '145', '118', '172', '172', '37', '37', '64', '172', '64', '145', '91', '64', '91', '226', '253', '118', '118', '118', '136', '145', '199', '118', '136', '172', '172', '37', '64', '172', '64', '145', '91', '64', '91', '226', '253', '118', '118', '118', '136', '136', '199', '145', '118', '172', '172', '37', '64', '136', '253', '93', '99');
INSERT INTO "sp" VALUES ('28', '38', '66', '178', '66', '150', '94', '66', '94', '234', '262', '122', '122', '122', '141', '141', '206', '150', '122', '178', '178', '38', '38', '66', '178', '66', '150', '94', '66', '94', '234', '262', '122', '122', '122', '141', '150', '206', '122', '141', '178', '178', '38', '66', '178', '66', '150', '94', '66', '94', '234', '262', '122', '122', '122', '141', '141', '206', '150', '122', '178', '178', '38', '66', '141', '262', '97', '103');
INSERT INTO "sp" VALUES ('29', '39', '68', '184', '68', '155', '97', '68', '97', '242', '271', '126', '126', '126', '146', '146', '213', '155', '126', '184', '184', '39', '39', '68', '184', '68', '155', '97', '68', '97', '242', '271', '126', '126', '126', '146', '155', '213', '126', '146', '184', '184', '39', '68', '184', '68', '155', '97', '68', '97', '242', '271', '126', '126', '126', '146', '146', '213', '155', '126', '184', '184', '39', '68', '146', '271', '101', '107');
INSERT INTO "sp" VALUES ('30', '40', '70', '190', '70', '160', '100', '70', '100', '250', '280', '130', '130', '130', '151', '151', '220', '160', '130', '190', '190', '40', '40', '70', '190', '70', '160', '100', '70', '100', '250', '280', '130', '130', '130', '151', '160', '220', '130', '151', '190', '190', '40', '70', '190', '70', '160', '100', '70', '100', '250', '280', '130', '130', '130', '151', '151', '220', '160', '130', '190', '190', '40', '70', '151', '280', '105', '111');
INSERT INTO "sp" VALUES ('31', '41', '72', '196', '72', '165', '103', '72', '103', '258', '289', '134', '134', '134', '155', '155', '227', '165', '134', '196', '196', '41', '41', '72', '196', '72', '165', '103', '72', '103', '258', '289', '134', '134', '134', '155', '165', '227', '134', '155', '196', '196', '41', '72', '196', '72', '165', '103', '72', '103', '258', '289', '134', '134', '134', '155', '155', '227', '165', '134', '196', '196', '41', '72', '155', '289', '109', '115');
INSERT INTO "sp" VALUES ('32', '42', '74', '202', '74', '170', '106', '74', '106', '266', '298', '138', '138', '138', '160', '160', '234', '170', '138', '202', '202', '42', '42', '74', '202', '74', '170', '106', '74', '106', '266', '298', '138', '138', '138', '160', '170', '234', '138', '160', '202', '202', '42', '74', '202', '74', '170', '106', '74', '106', '266', '298', '138', '138', '138', '160', '160', '234', '170', '138', '202', '202', '42', '74', '160', '298', '113', '119');
INSERT INTO "sp" VALUES ('33', '43', '76', '208', '76', '175', '109', '76', '109', '274', '307', '142', '142', '142', '165', '165', '241', '175', '142', '208', '208', '43', '43', '76', '208', '76', '175', '109', '76', '109', '274', '307', '142', '142', '142', '165', '175', '241', '142', '165', '208', '208', '43', '76', '208', '76', '175', '109', '76', '109', '274', '307', '142', '142', '142', '165', '165', '241', '175', '142', '208', '208', '43', '76', '165', '307', '117', '123');
INSERT INTO "sp" VALUES ('34', '44', '78', '214', '78', '180', '112', '78', '112', '282', '316', '146', '146', '146', '169', '169', '248', '180', '146', '214', '214', '44', '44', '78', '214', '78', '180', '112', '78', '112', '282', '316', '146', '146', '146', '169', '180', '248', '146', '169', '214', '214', '44', '78', '214', '78', '180', '112', '78', '112', '282', '316', '146', '146', '146', '169', '169', '248', '180', '146', '214', '214', '44', '78', '169', '316', '121', '127');
INSERT INTO "sp" VALUES ('35', '45', '80', '220', '80', '185', '115', '80', '115', '290', '325', '150', '150', '150', '174', '174', '255', '185', '150', '220', '220', '45', '45', '80', '220', '80', '185', '115', '80', '115', '290', '325', '150', '150', '150', '174', '185', '255', '150', '174', '220', '220', '45', '80', '220', '80', '185', '115', '80', '115', '290', '325', '150', '150', '150', '174', '174', '255', '185', '150', '220', '220', '45', '80', '174', '325', '125', '131');
INSERT INTO "sp" VALUES ('36', '46', '82', '226', '82', '190', '118', '82', '118', '298', '334', '154', '154', '154', '179', '179', '262', '190', '154', '226', '226', '46', '46', '82', '226', '82', '190', '118', '82', '118', '298', '334', '154', '154', '154', '179', '190', '262', '154', '179', '226', '226', '46', '82', '226', '82', '190', '118', '82', '118', '298', '334', '154', '154', '154', '179', '179', '262', '190', '154', '226', '226', '46', '82', '179', '334', '129', '135');
INSERT INTO "sp" VALUES ('37', '47', '84', '232', '84', '195', '121', '84', '121', '306', '343', '158', '158', '158', '183', '183', '269', '195', '158', '232', '232', '47', '47', '84', '232', '84', '195', '121', '84', '121', '306', '343', '158', '158', '158', '183', '195', '269', '158', '183', '232', '232', '47', '84', '232', '84', '195', '121', '84', '121', '306', '343', '158', '158', '158', '183', '183', '269', '195', '158', '232', '232', '47', '84', '183', '343', '131', '139');
INSERT INTO "sp" VALUES ('38', '48', '86', '238', '86', '200', '124', '86', '124', '314', '352', '162', '162', '162', '188', '188', '276', '200', '162', '238', '238', '48', '48', '86', '238', '86', '200', '124', '86', '124', '314', '352', '162', '162', '162', '188', '200', '276', '162', '188', '238', '238', '48', '86', '238', '86', '200', '124', '86', '124', '314', '352', '162', '162', '162', '188', '188', '276', '200', '162', '238', '238', '48', '86', '188', '352', '135', '143');
INSERT INTO "sp" VALUES ('39', '49', '88', '244', '88', '205', '127', '88', '127', '322', '361', '166', '166', '166', '193', '193', '283', '205', '166', '244', '244', '49', '49', '88', '244', '88', '205', '127', '88', '127', '322', '361', '166', '166', '166', '193', '205', '283', '166', '193', '244', '244', '49', '88', '244', '88', '205', '127', '88', '127', '322', '361', '166', '166', '166', '193', '193', '283', '205', '166', '244', '244', '49', '88', '193', '361', '139', '147');
INSERT INTO "sp" VALUES ('40', '50', '90', '250', '90', '210', '130', '90', '130', '330', '370', '170', '170', '170', '198', '198', '290', '210', '170', '250', '250', '50', '50', '90', '250', '90', '210', '130', '90', '130', '330', '370', '170', '170', '170', '198', '210', '290', '170', '198', '250', '250', '50', '90', '250', '90', '210', '130', '90', '130', '330', '370', '170', '170', '170', '198', '198', '290', '210', '170', '250', '250', '50', '90', '198', '370', '141', '151');
INSERT INTO "sp" VALUES ('41', '51', '92', '256', '92', '215', '133', '92', '133', '338', '379', '174', '174', '174', '202', '202', '297', '215', '174', '256', '256', '51', '51', '92', '256', '92', '215', '133', '92', '133', '338', '379', '174', '174', '174', '202', '215', '297', '174', '202', '256', '256', '51', '92', '256', '92', '215', '133', '92', '133', '338', '379', '174', '174', '174', '202', '202', '297', '215', '174', '256', '256', '51', '92', '202', '379', '145', '156');
INSERT INTO "sp" VALUES ('42', '52', '94', '262', '94', '220', '136', '94', '136', '346', '388', '178', '178', '178', '207', '207', '304', '220', '178', '262', '262', '52', '52', '94', '262', '94', '220', '136', '94', '136', '346', '388', '178', '178', '178', '207', '220', '304', '178', '207', '262', '262', '52', '94', '262', '94', '220', '136', '94', '136', '346', '388', '178', '178', '178', '207', '207', '304', '220', '178', '262', '262', '52', '94', '207', '388', '149', '161');
INSERT INTO "sp" VALUES ('43', '53', '96', '268', '96', '225', '139', '96', '139', '354', '397', '182', '182', '182', '212', '212', '311', '225', '182', '268', '268', '53', '53', '96', '268', '96', '225', '139', '96', '139', '354', '397', '182', '182', '182', '212', '225', '311', '182', '212', '268', '268', '53', '96', '268', '96', '225', '139', '96', '139', '354', '397', '182', '182', '182', '212', '212', '311', '225', '182', '268', '268', '53', '96', '212', '397', '153', '166');
INSERT INTO "sp" VALUES ('44', '54', '98', '274', '98', '230', '142', '98', '142', '362', '406', '186', '186', '186', '216', '216', '318', '230', '186', '274', '274', '54', '54', '98', '274', '98', '230', '142', '98', '142', '362', '406', '186', '186', '186', '216', '230', '318', '186', '216', '274', '274', '54', '98', '274', '98', '230', '142', '98', '142', '362', '406', '186', '186', '186', '216', '216', '318', '230', '186', '274', '274', '54', '98', '216', '406', '157', '171');
INSERT INTO "sp" VALUES ('45', '55', '100', '280', '100', '235', '145', '100', '145', '370', '415', '190', '190', '190', '221', '221', '325', '235', '190', '280', '280', '55', '55', '100', '280', '100', '235', '145', '100', '145', '370', '415', '190', '190', '190', '221', '235', '325', '190', '221', '280', '280', '55', '100', '280', '100', '235', '145', '100', '145', '370', '415', '190', '190', '190', '221', '221', '325', '235', '190', '280', '280', '55', '100', '221', '415', '161', '176');
INSERT INTO "sp" VALUES ('46', '56', '102', '286', '102', '240', '148', '102', '148', '378', '424', '194', '194', '194', '226', '226', '332', '240', '194', '286', '286', '56', '56', '102', '286', '102', '240', '148', '102', '148', '378', '424', '194', '194', '194', '226', '240', '332', '194', '226', '286', '286', '56', '102', '286', '102', '240', '148', '102', '148', '378', '424', '194', '194', '194', '226', '226', '332', '240', '194', '286', '286', '56', '102', '226', '424', '165', '181');
INSERT INTO "sp" VALUES ('47', '57', '104', '292', '104', '245', '151', '104', '151', '386', '433', '198', '198', '198', '230', '230', '339', '245', '198', '292', '292', '57', '57', '104', '292', '104', '245', '151', '104', '151', '386', '433', '198', '198', '198', '230', '245', '339', '198', '230', '292', '292', '57', '104', '292', '104', '245', '151', '104', '151', '386', '433', '198', '198', '198', '230', '230', '339', '245', '198', '292', '292', '57', '104', '230', '433', '169', '186');
INSERT INTO "sp" VALUES ('48', '58', '106', '298', '106', '250', '154', '106', '154', '394', '442', '202', '202', '202', '235', '235', '346', '250', '202', '298', '298', '58', '58', '106', '298', '106', '250', '154', '106', '154', '394', '442', '202', '202', '202', '235', '250', '346', '202', '235', '298', '298', '58', '106', '298', '106', '250', '154', '106', '154', '394', '442', '202', '202', '202', '235', '235', '346', '250', '202', '298', '298', '58', '106', '235', '442', '173', '191');
INSERT INTO "sp" VALUES ('49', '59', '108', '304', '108', '255', '157', '108', '157', '402', '451', '206', '206', '206', '240', '240', '353', '255', '206', '304', '304', '59', '59', '108', '304', '108', '255', '157', '108', '157', '402', '451', '206', '206', '206', '240', '255', '353', '206', '240', '304', '304', '59', '108', '304', '108', '255', '157', '108', '157', '402', '451', '206', '206', '206', '240', '240', '353', '255', '206', '304', '304', '59', '108', '240', '451', '177', '196');
INSERT INTO "sp" VALUES ('50', '60', '110', '310', '110', '260', '160', '110', '160', '410', '460', '210', '210', '210', '245', '245', '360', '260', '210', '310', '310', '60', '60', '110', '310', '110', '260', '160', '110', '160', '410', '460', '210', '210', '210', '245', '260', '360', '210', '245', '310', '310', '60', '110', '310', '110', '260', '160', '110', '160', '410', '460', '210', '210', '210', '245', '245', '360', '260', '210', '310', '310', '60', '110', '245', '460', '181', '201');
INSERT INTO "sp" VALUES ('51', '61', '112', '316', '112', '265', '163', '112', '163', '418', '469', '214', '214', '214', '249', '249', '367', '265', '214', '316', '316', '61', '61', '112', '316', '112', '265', '163', '112', '163', '418', '469', '214', '214', '214', '249', '265', '367', '214', '249', '316', '316', '61', '112', '316', '112', '265', '163', '112', '163', '418', '469', '214', '214', '214', '249', '249', '367', '265', '214', '316', '316', '61', '112', '249', '469', '186', '206');
INSERT INTO "sp" VALUES ('52', '62', '114', '322', '114', '270', '166', '114', '166', '426', '478', '218', '218', '218', '254', '254', '374', '270', '218', '322', '322', '62', '62', '114', '322', '114', '270', '166', '114', '166', '426', '478', '218', '218', '218', '254', '270', '374', '218', '254', '322', '322', '62', '114', '322', '114', '270', '166', '114', '166', '426', '478', '218', '218', '218', '254', '254', '374', '270', '218', '322', '322', '62', '114', '254', '478', '191', '211');
INSERT INTO "sp" VALUES ('53', '63', '116', '328', '116', '275', '169', '116', '169', '434', '487', '222', '222', '222', '259', '259', '381', '275', '222', '328', '328', '63', '63', '116', '328', '116', '275', '169', '116', '169', '434', '487', '222', '222', '222', '259', '275', '381', '222', '259', '328', '328', '63', '116', '328', '116', '275', '169', '116', '169', '434', '487', '222', '222', '222', '259', '259', '381', '275', '222', '328', '328', '63', '116', '259', '487', '196', '216');
INSERT INTO "sp" VALUES ('54', '64', '118', '334', '118', '280', '172', '118', '172', '442', '496', '226', '226', '226', '263', '263', '388', '280', '226', '334', '334', '64', '64', '118', '334', '118', '280', '172', '118', '172', '442', '496', '226', '226', '226', '263', '280', '388', '226', '263', '334', '334', '64', '118', '334', '118', '280', '172', '118', '172', '442', '496', '226', '226', '226', '263', '263', '388', '280', '226', '334', '334', '64', '118', '263', '496', '201', '221');
INSERT INTO "sp" VALUES ('55', '65', '120', '340', '120', '285', '175', '120', '175', '450', '505', '230', '230', '230', '268', '268', '395', '285', '230', '340', '340', '65', '65', '120', '340', '120', '285', '175', '120', '175', '450', '505', '230', '230', '230', '268', '285', '395', '230', '268', '340', '340', '65', '120', '340', '120', '285', '175', '120', '175', '450', '505', '230', '230', '230', '268', '268', '395', '285', '230', '340', '340', '65', '120', '268', '505', '206', '226');
INSERT INTO "sp" VALUES ('56', '66', '122', '346', '122', '290', '178', '122', '178', '458', '514', '234', '234', '234', '273', '273', '402', '290', '234', '346', '346', '66', '66', '122', '346', '122', '290', '178', '122', '178', '458', '514', '234', '234', '234', '273', '290', '402', '234', '273', '346', '346', '66', '122', '346', '122', '290', '178', '122', '178', '458', '514', '234', '234', '234', '273', '273', '402', '290', '234', '346', '346', '66', '122', '273', '514', '211', '231');
INSERT INTO "sp" VALUES ('57', '67', '124', '352', '124', '295', '181', '124', '181', '466', '523', '238', '238', '238', '277', '277', '409', '295', '238', '352', '352', '67', '67', '124', '352', '124', '295', '181', '124', '181', '466', '523', '238', '238', '238', '277', '295', '409', '238', '277', '352', '352', '67', '124', '352', '124', '295', '181', '124', '181', '466', '523', '238', '238', '238', '277', '277', '409', '295', '238', '352', '352', '67', '124', '277', '523', '216', '236');
INSERT INTO "sp" VALUES ('58', '68', '126', '358', '126', '300', '184', '126', '184', '474', '532', '242', '242', '242', '282', '282', '416', '300', '242', '358', '358', '68', '68', '126', '358', '126', '300', '184', '126', '184', '474', '532', '242', '242', '242', '282', '300', '416', '242', '282', '358', '358', '68', '126', '358', '126', '300', '184', '126', '184', '474', '532', '242', '242', '242', '282', '282', '416', '300', '242', '358', '358', '68', '126', '282', '532', '221', '241');
INSERT INTO "sp" VALUES ('59', '69', '128', '364', '128', '305', '187', '128', '187', '482', '541', '246', '246', '246', '287', '287', '423', '305', '246', '364', '364', '69', '69', '128', '364', '128', '305', '187', '128', '187', '482', '541', '246', '246', '246', '287', '305', '423', '246', '287', '364', '364', '69', '128', '364', '128', '305', '187', '128', '187', '482', '541', '246', '246', '246', '287', '287', '423', '305', '246', '364', '364', '69', '128', '287', '541', '226', '246');
INSERT INTO "sp" VALUES ('60', '70', '130', '370', '130', '310', '190', '130', '190', '490', '550', '250', '250', '250', '292', '292', '430', '310', '250', '370', '370', '70', '70', '130', '370', '130', '310', '190', '130', '190', '490', '550', '250', '250', '250', '292', '310', '430', '250', '292', '370', '370', '70', '130', '370', '130', '310', '190', '130', '190', '490', '550', '250', '250', '250', '292', '292', '430', '310', '250', '370', '370', '70', '130', '292', '550', '231', '251');
INSERT INTO "sp" VALUES ('61', '71', '132', '376', '132', '315', '193', '132', '193', '498', '559', '254', '254', '254', '296', '296', '437', '315', '254', '376', '376', '71', '71', '132', '376', '132', '315', '193', '132', '193', '498', '559', '254', '254', '254', '296', '315', '437', '254', '296', '376', '376', '71', '132', '376', '132', '315', '193', '132', '193', '498', '559', '254', '254', '254', '296', '296', '437', '315', '254', '376', '376', '71', '132', '296', '559', '236', '257');
INSERT INTO "sp" VALUES ('62', '72', '134', '382', '134', '320', '196', '134', '196', '506', '568', '258', '258', '258', '301', '301', '444', '320', '258', '382', '382', '72', '72', '134', '382', '134', '320', '196', '134', '196', '506', '568', '258', '258', '258', '301', '320', '444', '258', '301', '382', '382', '72', '134', '382', '134', '320', '196', '134', '196', '506', '568', '258', '258', '258', '301', '301', '444', '320', '258', '382', '382', '72', '134', '301', '568', '241', '263');
INSERT INTO "sp" VALUES ('63', '73', '136', '388', '136', '325', '199', '136', '199', '514', '577', '262', '262', '262', '306', '306', '451', '325', '262', '388', '388', '73', '73', '136', '388', '136', '325', '199', '136', '199', '514', '577', '262', '262', '262', '306', '325', '451', '262', '306', '388', '388', '73', '136', '388', '136', '325', '199', '136', '199', '514', '577', '262', '262', '262', '306', '306', '451', '325', '262', '388', '388', '73', '136', '306', '577', '246', '269');
INSERT INTO "sp" VALUES ('64', '74', '138', '394', '138', '330', '202', '138', '202', '522', '586', '266', '266', '266', '310', '310', '458', '330', '266', '394', '394', '74', '74', '138', '394', '138', '330', '202', '138', '202', '522', '586', '266', '266', '266', '310', '330', '458', '266', '310', '394', '394', '74', '138', '394', '138', '330', '202', '138', '202', '522', '586', '266', '266', '266', '310', '310', '458', '330', '266', '394', '394', '74', '138', '310', '586', '251', '275');
INSERT INTO "sp" VALUES ('65', '75', '140', '400', '140', '335', '205', '140', '205', '530', '595', '270', '270', '270', '315', '315', '465', '335', '270', '400', '400', '75', '75', '140', '400', '140', '335', '205', '140', '205', '530', '595', '270', '270', '270', '315', '335', '465', '270', '315', '400', '400', '75', '140', '400', '140', '335', '205', '140', '205', '530', '595', '270', '270', '270', '315', '315', '465', '335', '270', '400', '400', '75', '140', '315', '595', '256', '281');
INSERT INTO "sp" VALUES ('66', '76', '142', '406', '142', '340', '208', '142', '208', '538', '604', '274', '274', '274', '320', '320', '472', '340', '274', '406', '406', '76', '76', '142', '406', '142', '340', '208', '142', '208', '538', '604', '274', '274', '274', '320', '340', '472', '274', '320', '406', '406', '76', '142', '406', '142', '340', '208', '142', '208', '538', '604', '274', '274', '274', '320', '320', '472', '340', '274', '406', '406', '76', '142', '320', '604', '261', '287');
INSERT INTO "sp" VALUES ('67', '77', '144', '412', '144', '345', '211', '144', '211', '546', '613', '278', '278', '278', '324', '324', '479', '345', '278', '412', '412', '77', '77', '144', '412', '144', '345', '211', '144', '211', '546', '613', '278', '278', '278', '324', '345', '479', '278', '324', '412', '412', '77', '144', '412', '144', '345', '211', '144', '211', '546', '613', '278', '278', '278', '324', '324', '479', '345', '278', '412', '412', '77', '144', '324', '613', '266', '293');
INSERT INTO "sp" VALUES ('68', '78', '146', '418', '146', '350', '214', '146', '214', '554', '622', '282', '282', '282', '329', '329', '486', '350', '282', '418', '418', '78', '78', '146', '418', '146', '350', '214', '146', '214', '554', '622', '282', '282', '282', '329', '350', '486', '282', '329', '418', '418', '78', '146', '418', '146', '350', '214', '146', '214', '554', '622', '282', '282', '282', '329', '329', '486', '350', '282', '418', '418', '78', '146', '329', '622', '271', '299');
INSERT INTO "sp" VALUES ('69', '79', '148', '424', '148', '355', '217', '148', '217', '562', '631', '286', '286', '286', '334', '334', '493', '355', '286', '424', '424', '79', '79', '148', '424', '148', '355', '217', '148', '217', '562', '631', '286', '286', '286', '334', '355', '493', '286', '334', '424', '424', '79', '148', '424', '148', '355', '217', '148', '217', '562', '631', '286', '286', '286', '334', '334', '493', '355', '286', '424', '424', '79', '148', '334', '631', '276', '305');
INSERT INTO "sp" VALUES ('70', '80', '150', '430', '150', '360', '220', '150', '220', '570', '640', '290', '290', '290', '339', '339', '500', '360', '290', '430', '430', '80', '80', '150', '430', '150', '360', '220', '150', '220', '570', '640', '290', '290', '290', '339', '360', '500', '290', '339', '430', '430', '80', '150', '430', '150', '360', '220', '150', '220', '570', '640', '290', '290', '290', '339', '339', '500', '360', '290', '430', '430', '80', '150', '339', '635', '281', '311');
INSERT INTO "sp" VALUES ('71', '81', '152', '436', '152', '365', '223', '152', '223', '578', '649', '294', '294', '294', '343', '343', '507', '365', '294', '436', '436', '81', '81', '152', '436', '152', '365', '223', '152', '223', '578', '649', '294', '294', '294', '343', '365', '507', '294', '343', '436', '436', '81', '152', '436', '152', '365', '223', '152', '223', '578', '649', '294', '294', '294', '343', '343', '507', '365', '294', '436', '436', '81', '151', '341', '640', '286', '317');
INSERT INTO "sp" VALUES ('72', '82', '154', '442', '154', '370', '226', '154', '226', '586', '658', '298', '298', '298', '348', '348', '514', '370', '298', '442', '442', '82', '82', '154', '442', '154', '370', '226', '154', '226', '586', '658', '298', '298', '298', '348', '370', '514', '298', '348', '442', '442', '82', '154', '442', '154', '370', '226', '154', '226', '586', '658', '298', '298', '298', '348', '348', '514', '370', '298', '442', '442', '82', '152', '343', '645', '291', '323');
INSERT INTO "sp" VALUES ('73', '83', '156', '448', '156', '375', '229', '156', '229', '594', '667', '302', '302', '302', '353', '353', '521', '375', '302', '448', '448', '83', '83', '156', '448', '156', '375', '229', '156', '229', '594', '667', '302', '302', '302', '353', '375', '521', '302', '353', '448', '448', '83', '156', '448', '156', '375', '229', '156', '229', '594', '667', '302', '302', '302', '353', '353', '521', '375', '302', '448', '448', '83', '153', '345', '650', '296', '329');
INSERT INTO "sp" VALUES ('74', '84', '158', '454', '158', '380', '232', '158', '232', '602', '676', '306', '306', '306', '357', '357', '528', '380', '306', '454', '454', '84', '84', '158', '454', '158', '380', '232', '158', '232', '602', '676', '306', '306', '306', '357', '380', '528', '306', '357', '454', '454', '84', '158', '454', '158', '380', '232', '158', '232', '602', '676', '306', '306', '306', '357', '357', '528', '380', '306', '454', '454', '84', '154', '347', '655', '301', '335');
INSERT INTO "sp" VALUES ('75', '85', '160', '460', '160', '385', '235', '160', '235', '610', '685', '310', '310', '310', '362', '362', '535', '385', '310', '460', '460', '85', '85', '160', '460', '160', '385', '235', '160', '235', '610', '685', '310', '310', '310', '362', '385', '535', '310', '362', '460', '460', '85', '160', '460', '160', '385', '235', '160', '235', '610', '685', '310', '310', '310', '362', '362', '535', '385', '310', '460', '460', '85', '155', '349', '660', '306', '341');
INSERT INTO "sp" VALUES ('76', '86', '162', '466', '162', '390', '238', '162', '238', '618', '694', '314', '314', '314', '367', '367', '542', '390', '314', '466', '466', '86', '86', '162', '466', '162', '390', '238', '162', '238', '618', '694', '314', '314', '314', '367', '390', '542', '314', '367', '466', '466', '86', '162', '466', '162', '390', '238', '162', '238', '618', '694', '314', '314', '314', '367', '367', '542', '390', '314', '466', '466', '86', '156', '351', '665', '312', '347');
INSERT INTO "sp" VALUES ('77', '87', '164', '472', '164', '395', '241', '164', '241', '626', '703', '318', '318', '318', '371', '371', '549', '395', '318', '472', '472', '87', '87', '164', '472', '164', '395', '241', '164', '241', '626', '703', '318', '318', '318', '371', '395', '549', '318', '371', '472', '472', '87', '164', '472', '164', '395', '241', '164', '241', '626', '703', '318', '318', '318', '371', '371', '549', '395', '318', '472', '472', '87', '157', '353', '670', '318', '353');
INSERT INTO "sp" VALUES ('78', '88', '166', '478', '166', '400', '244', '166', '244', '634', '712', '322', '322', '322', '376', '376', '556', '400', '322', '478', '478', '88', '88', '166', '478', '166', '400', '244', '166', '244', '634', '712', '322', '322', '322', '376', '400', '556', '322', '376', '478', '478', '88', '166', '478', '166', '400', '244', '166', '244', '634', '712', '322', '322', '322', '376', '376', '556', '400', '322', '478', '478', '88', '158', '355', '675', '324', '359');
INSERT INTO "sp" VALUES ('79', '89', '168', '484', '168', '405', '247', '168', '247', '642', '721', '326', '326', '326', '381', '381', '563', '405', '326', '484', '484', '89', '89', '168', '484', '168', '405', '247', '168', '247', '642', '721', '326', '326', '326', '381', '405', '563', '326', '381', '484', '484', '89', '168', '484', '168', '405', '247', '168', '247', '642', '721', '326', '326', '326', '381', '381', '563', '405', '326', '484', '484', '89', '159', '357', '680', '336', '365');
INSERT INTO "sp" VALUES ('80', '90', '170', '490', '170', '410', '250', '170', '250', '650', '730', '330', '330', '330', '386', '386', '570', '410', '330', '490', '490', '90', '90', '170', '490', '170', '410', '250', '170', '250', '650', '730', '330', '330', '330', '386', '410', '570', '330', '386', '490', '490', '90', '170', '490', '170', '410', '250', '170', '250', '650', '730', '330', '330', '330', '386', '386', '570', '410', '330', '490', '490', '90', '170', '386', '730', '342', '371');
INSERT INTO "sp" VALUES ('81', '91', '172', '496', '172', '415', '253', '172', '253', '658', '739', '334', '334', '334', '390', '390', '577', '415', '334', '496', '496', '91', '91', '172', '496', '172', '415', '253', '172', '253', '658', '739', '334', '334', '334', '390', '415', '577', '334', '390', '496', '496', '91', '172', '496', '172', '415', '253', '172', '253', '658', '739', '334', '334', '334', '390', '390', '577', '415', '334', '496', '496', '91', '171', '388', '735', '348', '378');
INSERT INTO "sp" VALUES ('82', '92', '174', '502', '174', '420', '256', '174', '256', '666', '748', '338', '338', '338', '395', '395', '584', '420', '338', '502', '502', '92', '92', '174', '502', '174', '420', '256', '174', '256', '666', '748', '338', '338', '338', '395', '420', '584', '338', '395', '502', '502', '92', '174', '502', '174', '420', '256', '174', '256', '666', '748', '338', '338', '338', '395', '395', '584', '420', '338', '502', '502', '92', '172', '390', '740', '354', '386');
INSERT INTO "sp" VALUES ('83', '93', '176', '508', '176', '425', '259', '176', '259', '674', '757', '342', '342', '342', '400', '400', '591', '425', '342', '508', '508', '93', '93', '176', '508', '176', '425', '259', '176', '259', '674', '757', '342', '342', '342', '400', '425', '591', '342', '400', '508', '508', '93', '176', '508', '176', '425', '259', '176', '259', '674', '757', '342', '342', '342', '400', '400', '591', '425', '342', '508', '508', '93', '173', '392', '745', '360', '394');
INSERT INTO "sp" VALUES ('84', '94', '178', '514', '178', '430', '262', '178', '262', '682', '766', '346', '346', '346', '404', '404', '598', '430', '346', '514', '514', '94', '94', '178', '514', '178', '430', '262', '178', '262', '682', '766', '346', '346', '346', '404', '430', '598', '346', '404', '514', '514', '94', '178', '514', '178', '430', '262', '178', '262', '682', '766', '346', '346', '346', '404', '404', '598', '430', '346', '514', '514', '94', '174', '394', '750', '366', '402');
INSERT INTO "sp" VALUES ('85', '95', '180', '520', '180', '435', '265', '180', '265', '690', '775', '350', '350', '350', '409', '409', '605', '435', '350', '520', '520', '95', '95', '180', '520', '180', '435', '265', '180', '265', '690', '775', '350', '350', '350', '409', '435', '605', '350', '409', '520', '520', '95', '180', '520', '180', '435', '265', '180', '265', '690', '775', '350', '350', '350', '409', '409', '605', '435', '350', '520', '520', '95', '175', '396', '755', '372', '410');
INSERT INTO "sp" VALUES ('86', '96', '182', '526', '182', '440', '268', '182', '268', '698', '784', '354', '354', '354', '414', '414', '612', '440', '354', '526', '526', '96', '96', '182', '526', '182', '440', '268', '182', '268', '698', '784', '354', '354', '354', '414', '440', '612', '354', '414', '526', '526', '96', '182', '526', '182', '440', '268', '182', '268', '698', '784', '354', '354', '354', '414', '414', '612', '440', '354', '526', '526', '96', '176', '398', '760', '378', '418');
INSERT INTO "sp" VALUES ('87', '97', '184', '532', '184', '445', '271', '184', '271', '706', '793', '358', '358', '358', '418', '418', '619', '445', '358', '532', '532', '97', '97', '184', '532', '184', '445', '271', '184', '271', '706', '793', '358', '358', '358', '418', '445', '619', '358', '418', '532', '532', '97', '184', '532', '184', '445', '271', '184', '271', '706', '793', '358', '358', '358', '418', '418', '619', '445', '358', '532', '532', '97', '177', '400', '765', '384', '426');
INSERT INTO "sp" VALUES ('88', '98', '186', '538', '186', '450', '274', '186', '274', '714', '802', '362', '362', '362', '423', '423', '626', '450', '362', '538', '538', '98', '98', '186', '538', '186', '450', '274', '186', '274', '714', '802', '362', '362', '362', '423', '450', '626', '362', '423', '538', '538', '98', '186', '538', '186', '450', '274', '186', '274', '714', '802', '362', '362', '362', '423', '423', '626', '450', '362', '538', '538', '98', '178', '402', '770', '390', '434');
INSERT INTO "sp" VALUES ('89', '99', '188', '544', '188', '455', '277', '188', '277', '722', '811', '366', '366', '366', '428', '428', '633', '455', '366', '544', '544', '99', '99', '188', '544', '188', '455', '277', '188', '277', '722', '811', '366', '366', '366', '428', '455', '633', '366', '428', '544', '544', '99', '188', '544', '188', '455', '277', '188', '277', '722', '811', '366', '366', '366', '428', '428', '633', '455', '366', '544', '544', '99', '188', '404', '775', '396', '442');
INSERT INTO "sp" VALUES ('90', '100', '190', '550', '190', '460', '280', '190', '280', '730', '820', '370', '370', '370', '433', '433', '640', '460', '370', '550', '550', '100', '100', '190', '550', '190', '460', '280', '190', '280', '730', '820', '370', '370', '370', '433', '460', '640', '370', '433', '550', '550', '100', '190', '550', '190', '460', '280', '190', '280', '730', '820', '370', '370', '370', '433', '433', '640', '460', '370', '550', '550', '100', '190', '430', '820', '402', '450');
INSERT INTO "sp" VALUES ('91', '101', '192', '556', '192', '465', '283', '192', '283', '738', '829', '374', '374', '374', '437', '437', '647', '465', '374', '556', '556', '101', '101', '192', '556', '192', '465', '283', '192', '283', '738', '829', '374', '374', '374', '437', '465', '647', '374', '437', '556', '556', '101', '192', '556', '192', '465', '283', '192', '283', '738', '829', '374', '374', '374', '437', '437', '647', '465', '374', '556', '556', '101', '190', '433', '825', '408', '458');
INSERT INTO "sp" VALUES ('92', '102', '194', '562', '194', '470', '286', '194', '286', '746', '838', '378', '378', '378', '442', '442', '654', '470', '378', '562', '562', '102', '102', '194', '562', '194', '470', '286', '194', '286', '746', '838', '378', '378', '378', '442', '470', '654', '378', '442', '562', '562', '102', '194', '562', '194', '470', '286', '194', '286', '746', '838', '378', '378', '378', '442', '442', '654', '470', '378', '562', '562', '102', '190', '436', '830', '414', '466');
INSERT INTO "sp" VALUES ('93', '103', '196', '568', '196', '475', '289', '196', '289', '754', '847', '382', '382', '382', '447', '447', '661', '475', '382', '568', '568', '103', '103', '196', '568', '196', '475', '289', '196', '289', '754', '847', '382', '382', '382', '447', '475', '661', '382', '447', '568', '568', '103', '196', '568', '196', '475', '289', '196', '289', '754', '847', '382', '382', '382', '447', '447', '661', '475', '382', '568', '568', '103', '190', '439', '845', '420', '474');
INSERT INTO "sp" VALUES ('94', '104', '198', '574', '198', '480', '292', '198', '292', '762', '856', '386', '386', '386', '451', '451', '668', '480', '386', '574', '574', '104', '104', '198', '574', '198', '480', '292', '198', '292', '762', '856', '386', '386', '386', '451', '480', '668', '386', '451', '574', '574', '104', '198', '574', '198', '480', '292', '198', '292', '762', '856', '386', '386', '386', '451', '451', '668', '480', '386', '574', '574', '104', '190', '442', '850', '426', '482');
INSERT INTO "sp" VALUES ('95', '105', '200', '580', '200', '485', '295', '200', '295', '770', '865', '390', '390', '390', '456', '456', '675', '485', '390', '580', '580', '105', '105', '200', '580', '200', '485', '295', '200', '295', '770', '865', '390', '390', '390', '456', '485', '675', '390', '456', '580', '580', '105', '200', '580', '200', '485', '295', '200', '295', '770', '865', '390', '390', '390', '456', '456', '675', '485', '390', '580', '580', '105', '190', '445', '855', '432', '490');
INSERT INTO "sp" VALUES ('96', '106', '202', '586', '202', '490', '298', '202', '298', '778', '874', '394', '394', '394', '461', '461', '682', '490', '394', '586', '586', '106', '106', '202', '586', '202', '490', '298', '202', '298', '778', '874', '394', '394', '394', '461', '490', '682', '394', '461', '586', '586', '106', '202', '586', '202', '490', '298', '202', '298', '778', '874', '394', '394', '394', '461', '461', '682', '490', '394', '586', '586', '106', '190', '448', '860', '438', '498');
INSERT INTO "sp" VALUES ('97', '107', '204', '592', '204', '495', '301', '204', '301', '786', '883', '398', '398', '398', '465', '465', '689', '495', '398', '592', '592', '107', '107', '204', '592', '204', '495', '301', '204', '301', '786', '883', '398', '398', '398', '465', '495', '689', '398', '465', '592', '592', '107', '204', '592', '204', '495', '301', '204', '301', '786', '883', '398', '398', '398', '465', '465', '689', '495', '398', '592', '592', '107', '190', '451', '865', '444', '506');
INSERT INTO "sp" VALUES ('98', '108', '206', '598', '206', '500', '304', '206', '304', '794', '892', '402', '402', '402', '470', '470', '696', '500', '402', '598', '598', '108', '108', '206', '598', '206', '500', '304', '206', '304', '794', '892', '402', '402', '402', '470', '500', '696', '402', '470', '598', '598', '108', '206', '598', '206', '500', '304', '206', '304', '794', '892', '402', '402', '402', '470', '470', '696', '500', '402', '598', '598', '108', '190', '454', '870', '450', '514');
INSERT INTO "sp" VALUES ('99', '109', '208', '604', '208', '505', '307', '208', '307', '802', '901', '406', '406', '406', '475', '475', '703', '505', '406', '604', '604', '109', '109', '208', '604', '208', '505', '307', '208', '307', '802', '901', '406', '406', '406', '475', '505', '703', '406', '475', '604', '604', '109', '208', '604', '208', '505', '307', '208', '307', '802', '901', '406', '406', '406', '475', '475', '703', '505', '406', '604', '604', '109', '200', '500', '900', '456', '522');
INSERT INTO "statpoints" VALUES ('0', '0');
INSERT INTO "statpoints" VALUES ('1', '48');
INSERT INTO "statpoints" VALUES ('2', '51');
INSERT INTO "statpoints" VALUES ('3', '54');
INSERT INTO "statpoints" VALUES ('4', '57');
INSERT INTO "statpoints" VALUES ('5', '60');
INSERT INTO "statpoints" VALUES ('6', '64');
INSERT INTO "statpoints" VALUES ('7', '68');
INSERT INTO "statpoints" VALUES ('8', '72');
INSERT INTO "statpoints" VALUES ('9', '76');
INSERT INTO "statpoints" VALUES ('10', '80');
INSERT INTO "statpoints" VALUES ('11', '85');
INSERT INTO "statpoints" VALUES ('12', '90');
INSERT INTO "statpoints" VALUES ('13', '95');
INSERT INTO "statpoints" VALUES ('14', '100');
INSERT INTO "statpoints" VALUES ('15', '105');
INSERT INTO "statpoints" VALUES ('16', '111');
INSERT INTO "statpoints" VALUES ('17', '117');
INSERT INTO "statpoints" VALUES ('18', '123');
INSERT INTO "statpoints" VALUES ('19', '129');
INSERT INTO "statpoints" VALUES ('20', '135');
INSERT INTO "statpoints" VALUES ('21', '142');
INSERT INTO "statpoints" VALUES ('22', '149');
INSERT INTO "statpoints" VALUES ('23', '156');
INSERT INTO "statpoints" VALUES ('24', '163');
INSERT INTO "statpoints" VALUES ('25', '170');
INSERT INTO "statpoints" VALUES ('26', '178');
INSERT INTO "statpoints" VALUES ('27', '186');
INSERT INTO "statpoints" VALUES ('28', '194');
INSERT INTO "statpoints" VALUES ('29', '202');
INSERT INTO "statpoints" VALUES ('30', '210');
INSERT INTO "statpoints" VALUES ('31', '219');
INSERT INTO "statpoints" VALUES ('32', '228');
INSERT INTO "statpoints" VALUES ('33', '237');
INSERT INTO "statpoints" VALUES ('34', '246');
INSERT INTO "statpoints" VALUES ('35', '255');
INSERT INTO "statpoints" VALUES ('36', '265');
INSERT INTO "statpoints" VALUES ('37', '275');
INSERT INTO "statpoints" VALUES ('38', '285');
INSERT INTO "statpoints" VALUES ('39', '295');
INSERT INTO "statpoints" VALUES ('40', '305');
INSERT INTO "statpoints" VALUES ('41', '316');
INSERT INTO "statpoints" VALUES ('42', '327');
INSERT INTO "statpoints" VALUES ('43', '338');
INSERT INTO "statpoints" VALUES ('44', '349');
INSERT INTO "statpoints" VALUES ('45', '360');
INSERT INTO "statpoints" VALUES ('46', '372');
INSERT INTO "statpoints" VALUES ('47', '384');
INSERT INTO "statpoints" VALUES ('48', '396');
INSERT INTO "statpoints" VALUES ('49', '408');
INSERT INTO "statpoints" VALUES ('50', '420');
INSERT INTO "statpoints" VALUES ('51', '433');
INSERT INTO "statpoints" VALUES ('52', '446');
INSERT INTO "statpoints" VALUES ('53', '459');
INSERT INTO "statpoints" VALUES ('54', '472');
INSERT INTO "statpoints" VALUES ('55', '485');
INSERT INTO "statpoints" VALUES ('56', '499');
INSERT INTO "statpoints" VALUES ('57', '513');
INSERT INTO "statpoints" VALUES ('58', '527');
INSERT INTO "statpoints" VALUES ('59', '541');
INSERT INTO "statpoints" VALUES ('60', '555');
INSERT INTO "statpoints" VALUES ('61', '570');
INSERT INTO "statpoints" VALUES ('62', '585');
INSERT INTO "statpoints" VALUES ('63', '600');
INSERT INTO "statpoints" VALUES ('64', '615');
INSERT INTO "statpoints" VALUES ('65', '630');
INSERT INTO "statpoints" VALUES ('66', '646');
INSERT INTO "statpoints" VALUES ('67', '662');
INSERT INTO "statpoints" VALUES ('68', '678');
INSERT INTO "statpoints" VALUES ('69', '694');
INSERT INTO "statpoints" VALUES ('70', '710');
INSERT INTO "statpoints" VALUES ('71', '727');
INSERT INTO "statpoints" VALUES ('72', '744');
INSERT INTO "statpoints" VALUES ('73', '761');
INSERT INTO "statpoints" VALUES ('74', '778');
INSERT INTO "statpoints" VALUES ('75', '795');
INSERT INTO "statpoints" VALUES ('76', '813');
INSERT INTO "statpoints" VALUES ('77', '831');
INSERT INTO "statpoints" VALUES ('78', '849');
INSERT INTO "statpoints" VALUES ('79', '867');
INSERT INTO "statpoints" VALUES ('80', '885');
INSERT INTO "statpoints" VALUES ('81', '904');
INSERT INTO "statpoints" VALUES ('82', '923');
INSERT INTO "statpoints" VALUES ('83', '942');
INSERT INTO "statpoints" VALUES ('84', '961');
INSERT INTO "statpoints" VALUES ('85', '980');
INSERT INTO "statpoints" VALUES ('86', '1000');
INSERT INTO "statpoints" VALUES ('87', '1020');
INSERT INTO "statpoints" VALUES ('88', '1040');
INSERT INTO "statpoints" VALUES ('89', '1060');
INSERT INTO "statpoints" VALUES ('90', '1080');
INSERT INTO "statpoints" VALUES ('91', '1101');
INSERT INTO "statpoints" VALUES ('92', '1122');
INSERT INTO "statpoints" VALUES ('93', '1143');
INSERT INTO "statpoints" VALUES ('94', '1164');
INSERT INTO "statpoints" VALUES ('95', '1185');
INSERT INTO "statpoints" VALUES ('96', '1207');
INSERT INTO "statpoints" VALUES ('97', '1229');
INSERT INTO "statpoints" VALUES ('98', '1251');
INSERT INTO "statpoints" VALUES ('99', '1273');
INSERT INTO "weight" VALUES ('20000', '28000', '22000', '26000', '24000', '28000', '24000', '28000', '26000', '24000', '30000', '27000', '24000', '28000', '24000', '24000', '30000', '26000', '27000', '27000', '20000', '20000', '28000', '22000', '26000', '24000', '28000', '24000', '28000', '26000', '24000', '30000', '27000', '24000', '28000', '24000', '24000', '30000', '26000', '27000', '27000', '20000', '28000', '22000', '26000', '24000', '28000', '24000', '28000', '26000', '24000', '30000', '27000', '24000', '28000', '24000', '24000', '30000', '26000', '27000', '27000', '20000', '28000', '28000', '28000', '26000');
INSERT INTO `baseexp` VALUES (1, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4);
INSERT INTO `baseexp` VALUES (2, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9);
INSERT INTO `baseexp` VALUES (3, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16);
INSERT INTO `baseexp` VALUES (4, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25);
INSERT INTO `baseexp` VALUES (5, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36);
INSERT INTO `baseexp` VALUES (6, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77);
INSERT INTO `baseexp` VALUES (7, 112, 112, 112, 112, 112, 112, 112, 112, 112, 112, 112, 112, 112, 112, 112, 112, 112, 112, 112, 112, 112, 123, 123, 123, 123, 123, 123, 123, 123, 123, 123, 123, 123, 123, 123, 123, 123, 123, 123, 123, 123, 123, 112, 112, 112, 112, 112, 112, 112, 112, 112, 112, 112, 112, 112, 112, 112, 112, 112, 112, 112, 112, 112, 112, 112, 112, 112);
INSERT INTO `baseexp` VALUES (8, 153, 153, 153, 153, 153, 153, 153, 153, 153, 153, 153, 153, 153, 153, 153, 153, 153, 153, 153, 153, 153, 168, 168, 168, 168, 168, 168, 168, 168, 168, 168, 168, 168, 168, 168, 168, 168, 168, 168, 168, 168, 168, 153, 153, 153, 153, 153, 153, 153, 153, 153, 153, 153, 153, 153, 153, 153, 153, 153, 153, 153, 153, 153, 153, 153, 153, 153);
INSERT INTO `baseexp` VALUES (9, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 220, 220, 220, 220, 220, 220, 220, 220, 220, 220, 220, 220, 220, 220, 220, 220, 220, 220, 220, 220, 220, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200);
INSERT INTO `baseexp` VALUES (10, 253, 253, 253, 253, 253, 253, 253, 253, 253, 253, 253, 253, 253, 253, 253, 253, 253, 253, 253, 253, 253, 278, 278, 278, 278, 278, 278, 278, 278, 278, 278, 278, 278, 278, 278, 278, 278, 278, 278, 278, 278, 278, 253, 253, 253, 253, 253, 253, 253, 253, 253, 253, 253, 253, 253, 253, 253, 253, 253, 253, 253, 253, 253, 253, 253, 253, 253);
INSERT INTO `baseexp` VALUES (11, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 400, 400, 400, 400, 400, 400, 400, 400, 400, 400, 400, 400, 400, 400, 400, 400, 400, 400, 400, 400, 400, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320);
INSERT INTO `baseexp` VALUES (12, 385, 385, 385, 385, 385, 385, 385, 385, 385, 385, 385, 385, 385, 385, 385, 385, 385, 385, 385, 385, 385, 481, 481, 481, 481, 481, 481, 481, 481, 481, 481, 481, 481, 481, 481, 481, 481, 481, 481, 481, 481, 481, 385, 385, 385, 385, 385, 385, 385, 385, 385, 385, 385, 385, 385, 385, 385, 385, 385, 385, 385, 385, 385, 385, 385, 385, 385);
INSERT INTO `baseexp` VALUES (13, 490, 490, 490, 490, 490, 490, 490, 490, 490, 490, 490, 490, 490, 490, 490, 490, 490, 490, 490, 490, 490, 613, 613, 613, 613, 613, 613, 613, 613, 613, 613, 613, 613, 613, 613, 613, 613, 613, 613, 613, 613, 613, 490, 490, 490, 490, 490, 490, 490, 490, 490, 490, 490, 490, 490, 490, 490, 490, 490, 490, 490, 490, 490, 490, 490, 490, 490);
INSERT INTO `baseexp` VALUES (14, 585, 585, 585, 585, 585, 585, 585, 585, 585, 585, 585, 585, 585, 585, 585, 585, 585, 585, 585, 585, 585, 731, 731, 731, 731, 731, 731, 731, 731, 731, 731, 731, 731, 731, 731, 731, 731, 731, 731, 731, 731, 731, 585, 585, 585, 585, 585, 585, 585, 585, 585, 585, 585, 585, 585, 585, 585, 585, 585, 585, 585, 585, 585, 585, 585, 585, 585);
INSERT INTO `baseexp` VALUES (15, 700, 700, 700, 700, 700, 700, 700, 700, 700, 700, 700, 700, 700, 700, 700, 700, 700, 700, 700, 700, 700, 875, 875, 875, 875, 875, 875, 875, 875, 875, 875, 875, 875, 875, 875, 875, 875, 875, 875, 875, 875, 875, 700, 700, 700, 700, 700, 700, 700, 700, 700, 700, 700, 700, 700, 700, 700, 700, 700, 700, 700, 700, 700, 700, 700, 700, 700);
INSERT INTO `baseexp` VALUES (16, 830, 830, 830, 830, 830, 830, 830, 830, 830, 830, 830, 830, 830, 830, 830, 830, 830, 830, 830, 830, 830, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 830, 830, 830, 830, 830, 830, 830, 830, 830, 830, 830, 830, 830, 830, 830, 830, 830, 830, 830, 830, 830, 830, 830, 830, 830);
INSERT INTO `baseexp` VALUES (17, 970, 970, 970, 970, 970, 970, 970, 970, 970, 970, 970, 970, 970, 970, 970, 970, 970, 970, 970, 970, 970, 1213, 1213, 1213, 1213, 1213, 1213, 1213, 1213, 1213, 1213, 1213, 1213, 1213, 1213, 1213, 1213, 1213, 1213, 1213, 1213, 1213, 970, 970, 970, 970, 970, 970, 970, 970, 970, 970, 970, 970, 970, 970, 970, 970, 970, 970, 970, 970, 970, 970, 970, 970, 970);
INSERT INTO `baseexp` VALUES (18, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1400, 1400, 1400, 1400, 1400, 1400, 1400, 1400, 1400, 1400, 1400, 1400, 1400, 1400, 1400, 1400, 1400, 1400, 1400, 1400, 1400, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120);
INSERT INTO `baseexp` VALUES (19, 1260, 1260, 1260, 1260, 1260, 1260, 1260, 1260, 1260, 1260, 1260, 1260, 1260, 1260, 1260, 1260, 1260, 1260, 1260, 1260, 1260, 1575, 1575, 1575, 1575, 1575, 1575, 1575, 1575, 1575, 1575, 1575, 1575, 1575, 1575, 1575, 1575, 1575, 1575, 1575, 1575, 1575, 1260, 1260, 1260, 1260, 1260, 1260, 1260, 1260, 1260, 1260, 1260, 1260, 1260, 1260, 1260, 1260, 1260, 1260, 1260, 1260, 1260, 1260, 1260, 1260, 1260);
INSERT INTO `baseexp` VALUES (20, 1420, 1420, 1420, 1420, 1420, 1420, 1420, 1420, 1420, 1420, 1420, 1420, 1420, 1420, 1420, 1420, 1420, 1420, 1420, 1420, 1420, 1775, 1775, 1775, 1775, 1775, 1775, 1775, 1775, 1775, 1775, 1775, 1775, 1775, 1775, 1775, 1775, 1775, 1775, 1775, 1775, 1775, 1420, 1420, 1420, 1420, 1420, 1420, 1420, 1420, 1420, 1420, 1420, 1420, 1420, 1420, 1420, 1420, 1420, 1420, 1420, 1420, 1420, 1420, 1420, 1420, 1420);
INSERT INTO `baseexp` VALUES (21, 1620, 1620, 1620, 1620, 1620, 1620, 1620, 1620, 1620, 1620, 1620, 1620, 1620, 1620, 1620, 1620, 1620, 1620, 1620, 1620, 1620, 2268, 2268, 2268, 2268, 2268, 2268, 2268, 2268, 2268, 2268, 2268, 2268, 2268, 2268, 2268, 2268, 2268, 2268, 2268, 2268, 2268, 1620, 1620, 1620, 1620, 1620, 1620, 1620, 1620, 1620, 1620, 1620, 1620, 1620, 1620, 1620, 1620, 1620, 1620, 1620, 1620, 1620, 1620, 1620, 1620, 1620);
INSERT INTO `baseexp` VALUES (22, 1860, 1860, 1860, 1860, 1860, 1860, 1860, 1860, 1860, 1860, 1860, 1860, 1860, 1860, 1860, 1860, 1860, 1860, 1860, 1860, 1860, 2604, 2604, 2604, 2604, 2604, 2604, 2604, 2604, 2604, 2604, 2604, 2604, 2604, 2604, 2604, 2604, 2604, 2604, 2604, 2604, 2604, 1860, 1860, 1860, 1860, 1860, 1860, 1860, 1860, 1860, 1860, 1860, 1860, 1860, 1860, 1860, 1860, 1860, 1860, 1860, 1860, 1860, 1860, 1860, 1860, 1860);
INSERT INTO `baseexp` VALUES (23, 1990, 1990, 1990, 1990, 1990, 1990, 1990, 1990, 1990, 1990, 1990, 1990, 1990, 1990, 1990, 1990, 1990, 1990, 1990, 1990, 1990, 2786, 2786, 2786, 2786, 2786, 2786, 2786, 2786, 2786, 2786, 2786, 2786, 2786, 2786, 2786, 2786, 2786, 2786, 2786, 2786, 2786, 1990, 1990, 1990, 1990, 1990, 1990, 1990, 1990, 1990, 1990, 1990, 1990, 1990, 1990, 1990, 1990, 1990, 1990, 1990, 1990, 1990, 1990, 1990, 1990, 1990);
INSERT INTO `baseexp` VALUES (24, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 3136, 3136, 3136, 3136, 3136, 3136, 3136, 3136, 3136, 3136, 3136, 3136, 3136, 3136, 3136, 3136, 3136, 3136, 3136, 3136, 3136, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240);
INSERT INTO `baseexp` VALUES (25, 2504, 2504, 2504, 2504, 2504, 2504, 2504, 2504, 2504, 2504, 2504, 2504, 2504, 2504, 2504, 2504, 2504, 2504, 2504, 2504, 2504, 3506, 3506, 3506, 3506, 3506, 3506, 3506, 3506, 3506, 3506, 3506, 3506, 3506, 3506, 3506, 3506, 3506, 3506, 3506, 3506, 3506, 2504, 2504, 2504, 2504, 2504, 2504, 2504, 2504, 2504, 2504, 2504, 2504, 2504, 2504, 2504, 2504, 2504, 2504, 2504, 2504, 2504, 2504, 2504, 2504, 2504);
INSERT INTO `baseexp` VALUES (26, 2950, 2950, 2950, 2950, 2950, 2950, 2950, 2950, 2950, 2950, 2950, 2950, 2950, 2950, 2950, 2950, 2950, 2950, 2950, 2950, 2950, 4130, 4130, 4130, 4130, 4130, 4130, 4130, 4130, 4130, 4130, 4130, 4130, 4130, 4130, 4130, 4130, 4130, 4130, 4130, 4130, 4130, 2950, 2950, 2950, 2950, 2950, 2950, 2950, 2950, 2950, 2950, 2950, 2950, 2950, 2950, 2950, 2950, 2950, 2950, 2950, 2950, 2950, 2950, 2950, 2950, 2950);
INSERT INTO `baseexp` VALUES (27, 3426, 3426, 3426, 3426, 3426, 3426, 3426, 3426, 3426, 3426, 3426, 3426, 3426, 3426, 3426, 3426, 3426, 3426, 3426, 3426, 3426, 4796, 4796, 4796, 4796, 4796, 4796, 4796, 4796, 4796, 4796, 4796, 4796, 4796, 4796, 4796, 4796, 4796, 4796, 4796, 4796, 4796, 3426, 3426, 3426, 3426, 3426, 3426, 3426, 3426, 3426, 3426, 3426, 3426, 3426, 3426, 3426, 3426, 3426, 3426, 3426, 3426, 3426, 3426, 3426, 3426, 3426);
INSERT INTO `baseexp` VALUES (28, 3934, 3934, 3934, 3934, 3934, 3934, 3934, 3934, 3934, 3934, 3934, 3934, 3934, 3934, 3934, 3934, 3934, 3934, 3934, 3934, 3934, 5508, 5508, 5508, 5508, 5508, 5508, 5508, 5508, 5508, 5508, 5508, 5508, 5508, 5508, 5508, 5508, 5508, 5508, 5508, 5508, 5508, 3934, 3934, 3934, 3934, 3934, 3934, 3934, 3934, 3934, 3934, 3934, 3934, 3934, 3934, 3934, 3934, 3934, 3934, 3934, 3934, 3934, 3934, 3934, 3934, 3934);
INSERT INTO `baseexp` VALUES (29, 4474, 4474, 4474, 4474, 4474, 4474, 4474, 4474, 4474, 4474, 4474, 4474, 4474, 4474, 4474, 4474, 4474, 4474, 4474, 4474, 4474, 6264, 6264, 6264, 6264, 6264, 6264, 6264, 6264, 6264, 6264, 6264, 6264, 6264, 6264, 6264, 6264, 6264, 6264, 6264, 6264, 6264, 4474, 4474, 4474, 4474, 4474, 4474, 4474, 4474, 4474, 4474, 4474, 4474, 4474, 4474, 4474, 4474, 4474, 4474, 4474, 4474, 4474, 4474, 4474, 4474, 4474);
INSERT INTO `baseexp` VALUES (30, 6889, 6889, 6889, 6889, 6889, 6889, 6889, 6889, 6889, 6889, 6889, 6889, 6889, 6889, 6889, 6889, 6889, 6889, 6889, 6889, 6889, 9645, 9645, 9645, 9645, 9645, 9645, 9645, 9645, 9645, 9645, 9645, 9645, 9645, 9645, 9645, 9645, 9645, 9645, 9645, 9645, 9645, 6889, 6889, 6889, 6889, 6889, 6889, 6889, 6889, 6889, 6889, 6889, 6889, 6889, 6889, 6889, 6889, 6889, 6889, 6889, 6889, 6889, 6889, 6889, 6889, 6889);
INSERT INTO `baseexp` VALUES (31, 7995, 7995, 7995, 7995, 7995, 7995, 7995, 7995, 7995, 7995, 7995, 7995, 7995, 7995, 7995, 7995, 7995, 7995, 7995, 7995, 7995, 12392, 12392, 12392, 12392, 12392, 12392, 12392, 12392, 12392, 12392, 12392, 12392, 12392, 12392, 12392, 12392, 12392, 12392, 12392, 12392, 12392, 7995, 7995, 7995, 7995, 7995, 7995, 7995, 7995, 7995, 7995, 7995, 7995, 7995, 7995, 7995, 7995, 7995, 7995, 7995, 7995, 7995, 7995, 7995, 7995, 7995);
INSERT INTO `baseexp` VALUES (32, 9174, 9174, 9174, 9174, 9174, 9174, 9174, 9174, 9174, 9174, 9174, 9174, 9174, 9174, 9174, 9174, 9174, 9174, 9174, 9174, 9174, 14220, 14220, 14220, 14220, 14220, 14220, 14220, 14220, 14220, 14220, 14220, 14220, 14220, 14220, 14220, 14220, 14220, 14220, 14220, 14220, 14220, 9174, 9174, 9174, 9174, 9174, 9174, 9174, 9174, 9174, 9174, 9174, 9174, 9174, 9174, 9174, 9174, 9174, 9174, 9174, 9174, 9174, 9174, 9174, 9174, 9174);
INSERT INTO `baseexp` VALUES (33, 10425, 10425, 10425, 10425, 10425, 10425, 10425, 10425, 10425, 10425, 10425, 10425, 10425, 10425, 10425, 10425, 10425, 10425, 10425, 10425, 10425, 16159, 16159, 16159, 16159, 16159, 16159, 16159, 16159, 16159, 16159, 16159, 16159, 16159, 16159, 16159, 16159, 16159, 16159, 16159, 16159, 16159, 10425, 10425, 10425, 10425, 10425, 10425, 10425, 10425, 10425, 10425, 10425, 10425, 10425, 10425, 10425, 10425, 10425, 10425, 10425, 10425, 10425, 10425, 10425, 10425, 10425);
INSERT INTO `baseexp` VALUES (34, 11748, 11748, 11748, 11748, 11748, 11748, 11748, 11748, 11748, 11748, 11748, 11748, 11748, 11748, 11748, 11748, 11748, 11748, 11748, 11748, 11748, 18209, 18209, 18209, 18209, 18209, 18209, 18209, 18209, 18209, 18209, 18209, 18209, 18209, 18209, 18209, 18209, 18209, 18209, 18209, 18209, 18209, 11748, 11748, 11748, 11748, 11748, 11748, 11748, 11748, 11748, 11748, 11748, 11748, 11748, 11748, 11748, 11748, 11748, 11748, 11748, 11748, 11748, 11748, 11748, 11748, 11748);
INSERT INTO `baseexp` VALUES (35, 13967, 13967, 13967, 13967, 13967, 13967, 13967, 13967, 13967, 13967, 13967, 13967, 13967, 13967, 13967, 13967, 13967, 13967, 13967, 13967, 13967, 21649, 21649, 21649, 21649, 21649, 21649, 21649, 21649, 21649, 21649, 21649, 21649, 21649, 21649, 21649, 21649, 21649, 21649, 21649, 21649, 21649, 13967, 13967, 13967, 13967, 13967, 13967, 13967, 13967, 13967, 13967, 13967, 13967, 13967, 13967, 13967, 13967, 13967, 13967, 13967, 13967, 13967, 13967, 13967, 13967, 13967);
INSERT INTO `baseexp` VALUES (36, 15775, 15775, 15775, 15775, 15775, 15775, 15775, 15775, 15775, 15775, 15775, 15775, 15775, 15775, 15775, 15775, 15775, 15775, 15775, 15775, 15775, 24451, 24451, 24451, 24451, 24451, 24451, 24451, 24451, 24451, 24451, 24451, 24451, 24451, 24451, 24451, 24451, 24451, 24451, 24451, 24451, 24451, 15775, 15775, 15775, 15775, 15775, 15775, 15775, 15775, 15775, 15775, 15775, 15775, 15775, 15775, 15775, 15775, 15775, 15775, 15775, 15775, 15775, 15775, 15775, 15775, 15775);
INSERT INTO `baseexp` VALUES (37, 17678, 17678, 17678, 17678, 17678, 17678, 17678, 17678, 17678, 17678, 17678, 17678, 17678, 17678, 17678, 17678, 17678, 17678, 17678, 17678, 17678, 27401, 27401, 27401, 27401, 27401, 27401, 27401, 27401, 27401, 27401, 27401, 27401, 27401, 27401, 27401, 27401, 27401, 27401, 27401, 27401, 27401, 17678, 17678, 17678, 17678, 17678, 17678, 17678, 17678, 17678, 17678, 17678, 17678, 17678, 17678, 17678, 17678, 17678, 17678, 17678, 17678, 17678, 17678, 17678, 17678, 17678);
INSERT INTO `baseexp` VALUES (38, 19677, 19677, 19677, 19677, 19677, 19677, 19677, 19677, 19677, 19677, 19677, 19677, 19677, 19677, 19677, 19677, 19677, 19677, 19677, 19677, 19677, 30499, 30499, 30499, 30499, 30499, 30499, 30499, 30499, 30499, 30499, 30499, 30499, 30499, 30499, 30499, 30499, 30499, 30499, 30499, 30499, 30499, 19677, 19677, 19677, 19677, 19677, 19677, 19677, 19677, 19677, 19677, 19677, 19677, 19677, 19677, 19677, 19677, 19677, 19677, 19677, 19677, 19677, 19677, 19677, 19677, 19677);
INSERT INTO `baseexp` VALUES (39, 21773, 21773, 21773, 21773, 21773, 21773, 21773, 21773, 21773, 21773, 21773, 21773, 21773, 21773, 21773, 21773, 21773, 21773, 21773, 21773, 21773, 33748, 33748, 33748, 33748, 33748, 33748, 33748, 33748, 33748, 33748, 33748, 33748, 33748, 33748, 33748, 33748, 33748, 33748, 33748, 33748, 33748, 21773, 21773, 21773, 21773, 21773, 21773, 21773, 21773, 21773, 21773, 21773, 21773, 21773, 21773, 21773, 21773, 21773, 21773, 21773, 21773, 21773, 21773, 21773, 21773, 21773);
INSERT INTO `baseexp` VALUES (40, 30543, 30543, 30543, 30543, 30543, 30543, 30543, 30543, 30543, 30543, 30543, 30543, 30543, 30543, 30543, 30543, 30543, 30543, 30543, 30543, 30543, 47342, 47342, 47342, 47342, 47342, 47342, 47342, 47342, 47342, 47342, 47342, 47342, 47342, 47342, 47342, 47342, 47342, 47342, 47342, 47342, 47342, 30543, 30543, 30543, 30543, 30543, 30543, 30543, 30543, 30543, 30543, 30543, 30543, 30543, 30543, 30543, 30543, 30543, 30543, 30543, 30543, 30543, 30543, 30543, 30543, 30543);
INSERT INTO `baseexp` VALUES (41, 34212, 34212, 34212, 34212, 34212, 34212, 34212, 34212, 34212, 34212, 34212, 34212, 34212, 34212, 34212, 34212, 34212, 34212, 34212, 34212, 34212, 58160, 58160, 58160, 58160, 58160, 58160, 58160, 58160, 58160, 58160, 58160, 58160, 58160, 58160, 58160, 58160, 58160, 58160, 58160, 58160, 58160, 34212, 34212, 34212, 34212, 34212, 34212, 34212, 34212, 34212, 34212, 34212, 34212, 34212, 34212, 34212, 34212, 34212, 34212, 34212, 34212, 34212, 34212, 34212, 34212, 34212);
INSERT INTO `baseexp` VALUES (42, 38065, 38065, 38065, 38065, 38065, 38065, 38065, 38065, 38065, 38065, 38065, 38065, 38065, 38065, 38065, 38065, 38065, 38065, 38065, 38065, 38065, 64711, 64711, 64711, 64711, 64711, 64711, 64711, 64711, 64711, 64711, 64711, 64711, 64711, 64711, 64711, 64711, 64711, 64711, 64711, 64711, 64711, 38065, 38065, 38065, 38065, 38065, 38065, 38065, 38065, 38065, 38065, 38065, 38065, 38065, 38065, 38065, 38065, 38065, 38065, 38065, 38065, 38065, 38065, 38065, 38065, 38065);
INSERT INTO `baseexp` VALUES (43, 42102, 42102, 42102, 42102, 42102, 42102, 42102, 42102, 42102, 42102, 42102, 42102, 42102, 42102, 42102, 42102, 42102, 42102, 42102, 42102, 42102, 71573, 71573, 71573, 71573, 71573, 71573, 71573, 71573, 71573, 71573, 71573, 71573, 71573, 71573, 71573, 71573, 71573, 71573, 71573, 71573, 71573, 42102, 42102, 42102, 42102, 42102, 42102, 42102, 42102, 42102, 42102, 42102, 42102, 42102, 42102, 42102, 42102, 42102, 42102, 42102, 42102, 42102, 42102, 42102, 42102, 42102);
INSERT INTO `baseexp` VALUES (44, 46323, 46323, 46323, 46323, 46323, 46323, 46323, 46323, 46323, 46323, 46323, 46323, 46323, 46323, 46323, 46323, 46323, 46323, 46323, 46323, 46323, 78749, 78749, 78749, 78749, 78749, 78749, 78749, 78749, 78749, 78749, 78749, 78749, 78749, 78749, 78749, 78749, 78749, 78749, 78749, 78749, 78749, 46323, 46323, 46323, 46323, 46323, 46323, 46323, 46323, 46323, 46323, 46323, 46323, 46323, 46323, 46323, 46323, 46323, 46323, 46323, 46323, 46323, 46323, 46323, 46323, 46323);
INSERT INTO `baseexp` VALUES (45, 53026, 53026, 53026, 53026, 53026, 53026, 53026, 53026, 53026, 53026, 53026, 53026, 53026, 53026, 53026, 53026, 53026, 53026, 53026, 53026, 53026, 90144, 90144, 90144, 90144, 90144, 90144, 90144, 90144, 90144, 90144, 90144, 90144, 90144, 90144, 90144, 90144, 90144, 90144, 90144, 90144, 90144, 53026, 53026, 53026, 53026, 53026, 53026, 53026, 53026, 53026, 53026, 53026, 53026, 53026, 53026, 53026, 53026, 53026, 53026, 53026, 53026, 53026, 53026, 53026, 53026, 53026);
INSERT INTO `baseexp` VALUES (46, 58419, 58419, 58419, 58419, 58419, 58419, 58419, 58419, 58419, 58419, 58419, 58419, 58419, 58419, 58419, 58419, 58419, 58419, 58419, 58419, 58419, 99312, 99312, 99312, 99312, 99312, 99312, 99312, 99312, 99312, 99312, 99312, 99312, 99312, 99312, 99312, 99312, 99312, 99312, 99312, 99312, 99312, 58419, 58419, 58419, 58419, 58419, 58419, 58419, 58419, 58419, 58419, 58419, 58419, 58419, 58419, 58419, 58419, 58419, 58419, 58419, 58419, 58419, 58419, 58419, 58419, 58419);
INSERT INTO `baseexp` VALUES (47, 64041, 64041, 64041, 64041, 64041, 64041, 64041, 64041, 64041, 64041, 64041, 64041, 64041, 64041, 64041, 64041, 64041, 64041, 64041, 64041, 64041, 108870, 108870, 108870, 108870, 108870, 108870, 108870, 108870, 108870, 108870, 108870, 108870, 108870, 108870, 108870, 108870, 108870, 108870, 108870, 108870, 108870, 64041, 64041, 64041, 64041, 64041, 64041, 64041, 64041, 64041, 64041, 64041, 64041, 64041, 64041, 64041, 64041, 64041, 64041, 64041, 64041, 64041, 64041, 64041, 64041, 64041);
INSERT INTO `baseexp` VALUES (48, 69892, 69892, 69892, 69892, 69892, 69892, 69892, 69892, 69892, 69892, 69892, 69892, 69892, 69892, 69892, 69892, 69892, 69892, 69892, 69892, 69892, 118816, 118816, 118816, 118816, 118816, 118816, 118816, 118816, 118816, 118816, 118816, 118816, 118816, 118816, 118816, 118816, 118816, 118816, 118816, 118816, 118816, 69892, 69892, 69892, 69892, 69892, 69892, 69892, 69892, 69892, 69892, 69892, 69892, 69892, 69892, 69892, 69892, 69892, 69892, 69892, 69892, 69892, 69892, 69892, 69892, 69892);
INSERT INTO `baseexp` VALUES (49, 75973, 75973, 75973, 75973, 75973, 75973, 75973, 75973, 75973, 75973, 75973, 75973, 75973, 75973, 75973, 75973, 75973, 75973, 75973, 75973, 75973, 129154, 129154, 129154, 129154, 129154, 129154, 129154, 129154, 129154, 129154, 129154, 129154, 129154, 129154, 129154, 129154, 129154, 129154, 129154, 129154, 129154, 75973, 75973, 75973, 75973, 75973, 75973, 75973, 75973, 75973, 75973, 75973, 75973, 75973, 75973, 75973, 75973, 75973, 75973, 75973, 75973, 75973, 75973, 75973, 75973, 75973);
INSERT INTO `baseexp` VALUES (50, 102468, 102468, 102468, 102468, 102468, 102468, 102468, 102468, 102468, 102468, 102468, 102468, 102468, 102468, 102468, 102468, 102468, 102468, 102468, 102468, 102468, 174196, 174196, 174196, 174196, 174196, 174196, 174196, 174196, 174196, 174196, 174196, 174196, 174196, 174196, 174196, 174196, 174196, 174196, 174196, 174196, 174196, 102468, 102468, 102468, 102468, 102468, 102468, 102468, 102468, 102468, 102468, 102468, 102468, 102468, 102468, 102468, 102468, 102468, 102468, 102468, 102468, 102468, 102468, 102468, 102468, 102468);
INSERT INTO `baseexp` VALUES (51, 115254, 115254, 115254, 115254, 115254, 115254, 115254, 115254, 115254, 115254, 115254, 115254, 115254, 115254, 115254, 115254, 115254, 115254, 115254, 115254, 115254, 213220, 213220, 213220, 213220, 213220, 213220, 213220, 213220, 213220, 213220, 213220, 213220, 213220, 213220, 213220, 213220, 213220, 213220, 213220, 213220, 213220, 115254, 115254, 115254, 115254, 115254, 115254, 115254, 115254, 115254, 115254, 115254, 115254, 115254, 115254, 115254, 115254, 115254, 115254, 115254, 115254, 115254, 115254, 115254, 115254, 115254);
INSERT INTO `baseexp` VALUES (52, 128692, 128692, 128692, 128692, 128692, 128692, 128692, 128692, 128692, 128692, 128692, 128692, 128692, 128692, 128692, 128692, 128692, 128692, 128692, 128692, 128692, 238080, 238080, 238080, 238080, 238080, 238080, 238080, 238080, 238080, 238080, 238080, 238080, 238080, 238080, 238080, 238080, 238080, 238080, 238080, 238080, 238080, 128692, 128692, 128692, 128692, 128692, 128692, 128692, 128692, 128692, 128692, 128692, 128692, 128692, 128692, 128692, 128692, 128692, 128692, 128692, 128692, 128692, 128692, 128692, 128692, 128692);
INSERT INTO `baseexp` VALUES (53, 142784, 142784, 142784, 142784, 142784, 142784, 142784, 142784, 142784, 142784, 142784, 142784, 142784, 142784, 142784, 142784, 142784, 142784, 142784, 142784, 142784, 264150, 264150, 264150, 264150, 264150, 264150, 264150, 264150, 264150, 264150, 264150, 264150, 264150, 264150, 264150, 264150, 264150, 264150, 264150, 264150, 264150, 142784, 142784, 142784, 142784, 142784, 142784, 142784, 142784, 142784, 142784, 142784, 142784, 142784, 142784, 142784, 142784, 142784, 142784, 142784, 142784, 142784, 142784, 142784, 142784, 142784);
INSERT INTO `baseexp` VALUES (54, 157528, 157528, 157528, 157528, 157528, 157528, 157528, 157528, 157528, 157528, 157528, 157528, 157528, 157528, 157528, 157528, 157528, 157528, 157528, 157528, 157528, 291427, 291427, 291427, 291427, 291427, 291427, 291427, 291427, 291427, 291427, 291427, 291427, 291427, 291427, 291427, 291427, 291427, 291427, 291427, 291427, 291427, 157528, 157528, 157528, 157528, 157528, 157528, 157528, 157528, 157528, 157528, 157528, 157528, 157528, 157528, 157528, 157528, 157528, 157528, 157528, 157528, 157528, 157528, 157528, 157528, 157528);
INSERT INTO `baseexp` VALUES (55, 178184, 178184, 178184, 178184, 178184, 178184, 178184, 178184, 178184, 178184, 178184, 178184, 178184, 178184, 178184, 178184, 178184, 178184, 178184, 178184, 178184, 329640, 329640, 329640, 329640, 329640, 329640, 329640, 329640, 329640, 329640, 329640, 329640, 329640, 329640, 329640, 329640, 329640, 329640, 329640, 329640, 329640, 178184, 178184, 178184, 178184, 178184, 178184, 178184, 178184, 178184, 178184, 178184, 178184, 178184, 178184, 178184, 178184, 178184, 178184, 178184, 178184, 178184, 178184, 178184, 178184, 178184);
INSERT INTO `baseexp` VALUES (56, 196300, 196300, 196300, 196300, 196300, 196300, 196300, 196300, 196300, 196300, 196300, 196300, 196300, 196300, 196300, 196300, 196300, 196300, 196300, 196300, 196300, 363155, 363155, 363155, 363155, 363155, 363155, 363155, 363155, 363155, 363155, 363155, 363155, 363155, 363155, 363155, 363155, 363155, 363155, 363155, 363155, 363155, 196300, 196300, 196300, 196300, 196300, 196300, 196300, 196300, 196300, 196300, 196300, 196300, 196300, 196300, 196300, 196300, 196300, 196300, 196300, 196300, 196300, 196300, 196300, 196300, 196300);
INSERT INTO `baseexp` VALUES (57, 215198, 215198, 215198, 215198, 215198, 215198, 215198, 215198, 215198, 215198, 215198, 215198, 215198, 215198, 215198, 215198, 215198, 215198, 215198, 215198, 215198, 398116, 398116, 398116, 398116, 398116, 398116, 398116, 398116, 398116, 398116, 398116, 398116, 398116, 398116, 398116, 398116, 398116, 398116, 398116, 398116, 398116, 215198, 215198, 215198, 215198, 215198, 215198, 215198, 215198, 215198, 215198, 215198, 215198, 215198, 215198, 215198, 215198, 215198, 215198, 215198, 215198, 215198, 215198, 215198, 215198, 215198);
INSERT INTO `baseexp` VALUES (58, 234879, 234879, 234879, 234879, 234879, 234879, 234879, 234879, 234879, 234879, 234879, 234879, 234879, 234879, 234879, 234879, 234879, 234879, 234879, 234879, 234879, 434526, 434526, 434526, 434526, 434526, 434526, 434526, 434526, 434526, 434526, 434526, 434526, 434526, 434526, 434526, 434526, 434526, 434526, 434526, 434526, 434526, 234879, 234879, 234879, 234879, 234879, 234879, 234879, 234879, 234879, 234879, 234879, 234879, 234879, 234879, 234879, 234879, 234879, 234879, 234879, 234879, 234879, 234879, 234879, 234879, 234879);
INSERT INTO `baseexp` VALUES (59, 255341, 255341, 255341, 255341, 255341, 255341, 255341, 255341, 255341, 255341, 255341, 255341, 255341, 255341, 255341, 255341, 255341, 255341, 255341, 255341, 255341, 472381, 472381, 472381, 472381, 472381, 472381, 472381, 472381, 472381, 472381, 472381, 472381, 472381, 472381, 472381, 472381, 472381, 472381, 472381, 472381, 472381, 255341, 255341, 255341, 255341, 255341, 255341, 255341, 255341, 255341, 255341, 255341, 255341, 255341, 255341, 255341, 255341, 255341, 255341, 255341, 255341, 255341, 255341, 255341, 255341, 255341);
INSERT INTO `baseexp` VALUES (60, 330188, 330188, 330188, 330188, 330188, 330188, 330188, 330188, 330188, 330188, 330188, 330188, 330188, 330188, 330188, 330188, 330188, 330188, 330188, 330188, 330188, 610848, 610848, 610848, 610848, 610848, 610848, 610848, 610848, 610848, 610848, 610848, 610848, 610848, 610848, 610848, 610848, 610848, 610848, 610848, 610848, 610848, 330188, 330188, 330188, 330188, 330188, 330188, 330188, 330188, 330188, 330188, 330188, 330188, 330188, 330188, 330188, 330188, 330188, 330188, 330188, 330188, 330188, 330188, 330188, 330188, 330188);
INSERT INTO `baseexp` VALUES (61, 365914, 365914, 365914, 365914, 365914, 365914, 365914, 365914, 365914, 365914, 365914, 365914, 365914, 365914, 365914, 365914, 365914, 365914, 365914, 365914, 365914, 731828, 731828, 731828, 731828, 731828, 731828, 731828, 731828, 731828, 731828, 731828, 731828, 731828, 731828, 731828, 731828, 731828, 731828, 731828, 731828, 731828, 365914, 365914, 365914, 365914, 365914, 365914, 365914, 365914, 365914, 365914, 365914, 365914, 365914, 365914, 365914, 365914, 365914, 365914, 365914, 365914, 365914, 365914, 365914, 365914, 365914);
INSERT INTO `baseexp` VALUES (62, 403224, 403224, 403224, 403224, 403224, 403224, 403224, 403224, 403224, 403224, 403224, 403224, 403224, 403224, 403224, 403224, 403224, 403224, 403224, 403224, 403224, 806448, 806448, 806448, 806448, 806448, 806448, 806448, 806448, 806448, 806448, 806448, 806448, 806448, 806448, 806448, 806448, 806448, 806448, 806448, 806448, 806448, 403224, 403224, 403224, 403224, 403224, 403224, 403224, 403224, 403224, 403224, 403224, 403224, 403224, 403224, 403224, 403224, 403224, 403224, 403224, 403224, 403224, 403224, 403224, 403224, 403224);
INSERT INTO `baseexp` VALUES (63, 442116, 442116, 442116, 442116, 442116, 442116, 442116, 442116, 442116, 442116, 442116, 442116, 442116, 442116, 442116, 442116, 442116, 442116, 442116, 442116, 442116, 884232, 884232, 884232, 884232, 884232, 884232, 884232, 884232, 884232, 884232, 884232, 884232, 884232, 884232, 884232, 884232, 884232, 884232, 884232, 884232, 884232, 442116, 442116, 442116, 442116, 442116, 442116, 442116, 442116, 442116, 442116, 442116, 442116, 442116, 442116, 442116, 442116, 442116, 442116, 442116, 442116, 442116, 442116, 442116, 442116, 442116);
INSERT INTO `baseexp` VALUES (64, 482590, 482590, 482590, 482590, 482590, 482590, 482590, 482590, 482590, 482590, 482590, 482590, 482590, 482590, 482590, 482590, 482590, 482590, 482590, 482590, 482590, 965180, 965180, 965180, 965180, 965180, 965180, 965180, 965180, 965180, 965180, 965180, 965180, 965180, 965180, 965180, 965180, 965180, 965180, 965180, 965180, 965180, 482590, 482590, 482590, 482590, 482590, 482590, 482590, 482590, 482590, 482590, 482590, 482590, 482590, 482590, 482590, 482590, 482590, 482590, 482590, 482590, 482590, 482590, 482590, 482590, 482590);
INSERT INTO `baseexp` VALUES (65, 536948, 536948, 536948, 536948, 536948, 536948, 536948, 536948, 536948, 536948, 536948, 536948, 536948, 536948, 536948, 536948, 536948, 536948, 536948, 536948, 536948, 1073896, 1073896, 1073896, 1073896, 1073896, 1073896, 1073896, 1073896, 1073896, 1073896, 1073896, 1073896, 1073896, 1073896, 1073896, 1073896, 1073896, 1073896, 1073896, 1073896, 1073896, 536948, 536948, 536948, 536948, 536948, 536948, 536948, 536948, 536948, 536948, 536948, 536948, 536948, 536948, 536948, 536948, 536948, 536948, 536948, 536948, 536948, 536948, 536948, 536948, 536948);
INSERT INTO `baseexp` VALUES (66, 585191, 585191, 585191, 585191, 585191, 585191, 585191, 585191, 585191, 585191, 585191, 585191, 585191, 585191, 585191, 585191, 585191, 585191, 585191, 585191, 585191, 1170382, 1170382, 1170382, 1170382, 1170382, 1170382, 1170382, 1170382, 1170382, 1170382, 1170382, 1170382, 1170382, 1170382, 1170382, 1170382, 1170382, 1170382, 1170382, 1170382, 1170382, 585191, 585191, 585191, 585191, 585191, 585191, 585191, 585191, 585191, 585191, 585191, 585191, 585191, 585191, 585191, 585191, 585191, 585191, 585191, 585191, 585191, 585191, 585191, 585191, 585191);
INSERT INTO `baseexp` VALUES (67, 635278, 635278, 635278, 635278, 635278, 635278, 635278, 635278, 635278, 635278, 635278, 635278, 635278, 635278, 635278, 635278, 635278, 635278, 635278, 635278, 635278, 1270556, 1270556, 1270556, 1270556, 1270556, 1270556, 1270556, 1270556, 1270556, 1270556, 1270556, 1270556, 1270556, 1270556, 1270556, 1270556, 1270556, 1270556, 1270556, 1270556, 1270556, 635278, 635278, 635278, 635278, 635278, 635278, 635278, 635278, 635278, 635278, 635278, 635278, 635278, 635278, 635278, 635278, 635278, 635278, 635278, 635278, 635278, 635278, 635278, 635278, 635278);
INSERT INTO `baseexp` VALUES (68, 687211, 687211, 687211, 687211, 687211, 687211, 687211, 687211, 687211, 687211, 687211, 687211, 687211, 687211, 687211, 687211, 687211, 687211, 687211, 687211, 687211, 1374422, 1374422, 1374422, 1374422, 1374422, 1374422, 1374422, 1374422, 1374422, 1374422, 1374422, 1374422, 1374422, 1374422, 1374422, 1374422, 1374422, 1374422, 1374422, 1374422, 1374422, 687211, 687211, 687211, 687211, 687211, 687211, 687211, 687211, 687211, 687211, 687211, 687211, 687211, 687211, 687211, 687211, 687211, 687211, 687211, 687211, 687211, 687211, 687211, 687211, 687211);
INSERT INTO `baseexp` VALUES (69, 740988, 740988, 740988, 740988, 740988, 740988, 740988, 740988, 740988, 740988, 740988, 740988, 740988, 740988, 740988, 740988, 740988, 740988, 740988, 740988, 740988, 1481976, 1481976, 1481976, 1481976, 1481976, 1481976, 1481976, 1481976, 1481976, 1481976, 1481976, 1481976, 1481976, 1481976, 1481976, 1481976, 1481976, 1481976, 1481976, 1481976, 1481976, 740988, 740988, 740988, 740988, 740988, 740988, 740988, 740988, 740988, 740988, 740988, 740988, 740988, 740988, 740988, 740988, 740988, 740988, 740988, 740988, 740988, 740988, 740988, 740988, 740988);
INSERT INTO `baseexp` VALUES (70, 925400, 925400, 925400, 925400, 925400, 925400, 925400, 925400, 925400, 925400, 925400, 925400, 925400, 925400, 925400, 925400, 925400, 925400, 925400, 925400, 925400, 1850800, 1850800, 1850800, 1850800, 1850800, 1850800, 1850800, 1850800, 1850800, 1850800, 1850800, 1850800, 1850800, 1850800, 1850800, 1850800, 1850800, 1850800, 1850800, 1850800, 1850800, 925400, 925400, 925400, 925400, 925400, 925400, 925400, 925400, 925400, 925400, 925400, 925400, 925400, 925400, 925400, 925400, 925400, 925400, 925400, 925400, 925400, 925400, 925400, 925400, 925400);
INSERT INTO `baseexp` VALUES (71, 1473746, 1473746, 1473746, 1473746, 1473746, 1473746, 1473746, 1473746, 1473746, 1473746, 1473746, 1473746, 1473746, 1473746, 1473746, 1473746, 1473746, 1473746, 1473746, 1473746, 1473746, 3389616, 3389616, 3389616, 3389616, 3389616, 3389616, 3389616, 3389616, 3389616, 3389616, 3389616, 3389616, 3389616, 3389616, 3389616, 3389616, 3389616, 3389616, 3389616, 3389616, 3389616, 1473746, 1473746, 1473746, 1473746, 1473746, 1473746, 1473746, 1473746, 1473746, 1473746, 1473746, 1473746, 1473746, 1473746, 1473746, 1473746, 1473746, 1473746, 1473746, 1473746, 1473746, 1473746, 1473746, 1473746, 1473746);
INSERT INTO `baseexp` VALUES (72, 1594058, 1594058, 1594058, 1594058, 1594058, 1594058, 1594058, 1594058, 1594058, 1594058, 1594058, 1594058, 1594058, 1594058, 1594058, 1594058, 1594058, 1594058, 1594058, 1594058, 1594058, 3666333, 3666333, 3666333, 3666333, 3666333, 3666333, 3666333, 3666333, 3666333, 3666333, 3666333, 3666333, 3666333, 3666333, 3666333, 3666333, 3666333, 3666333, 3666333, 3666333, 3666333, 1594058, 1594058, 1594058, 1594058, 1594058, 1594058, 1594058, 1594058, 1594058, 1594058, 1594058, 1594058, 1594058, 1594058, 1594058, 1594058, 1594058, 1594058, 1594058, 1594058, 1594058, 1594058, 1594058, 1594058, 1594058);
INSERT INTO `baseexp` VALUES (73, 1718928, 1718928, 1718928, 1718928, 1718928, 1718928, 1718928, 1718928, 1718928, 1718928, 1718928, 1718928, 1718928, 1718928, 1718928, 1718928, 1718928, 1718928, 1718928, 1718928, 1718928, 3953534, 3953534, 3953534, 3953534, 3953534, 3953534, 3953534, 3953534, 3953534, 3953534, 3953534, 3953534, 3953534, 3953534, 3953534, 3953534, 3953534, 3953534, 3953534, 3953534, 3953534, 1718928, 1718928, 1718928, 1718928, 1718928, 1718928, 1718928, 1718928, 1718928, 1718928, 1718928, 1718928, 1718928, 1718928, 1718928, 1718928, 1718928, 1718928, 1718928, 1718928, 1718928, 1718928, 1718928, 1718928, 1718928);
INSERT INTO `baseexp` VALUES (74, 1848355, 1848355, 1848355, 1848355, 1848355, 1848355, 1848355, 1848355, 1848355, 1848355, 1848355, 1848355, 1848355, 1848355, 1848355, 1848355, 1848355, 1848355, 1848355, 1848355, 1848355, 4251217, 4251217, 4251217, 4251217, 4251217, 4251217, 4251217, 4251217, 4251217, 4251217, 4251217, 4251217, 4251217, 4251217, 4251217, 4251217, 4251217, 4251217, 4251217, 4251217, 4251217, 1848355, 1848355, 1848355, 1848355, 1848355, 1848355, 1848355, 1848355, 1848355, 1848355, 1848355, 1848355, 1848355, 1848355, 1848355, 1848355, 1848355, 1848355, 1848355, 1848355, 1848355, 1848355, 1848355, 1848355, 1848355);
INSERT INTO `baseexp` VALUES (75, 1982340, 1982340, 1982340, 1982340, 1982340, 1982340, 1982340, 1982340, 1982340, 1982340, 1982340, 1982340, 1982340, 1982340, 1982340, 1982340, 1982340, 1982340, 1982340, 1982340, 1982340, 4559382, 4559382, 4559382, 4559382, 4559382, 4559382, 4559382, 4559382, 4559382, 4559382, 4559382, 4559382, 4559382, 4559382, 4559382, 4559382, 4559382, 4559382, 4559382, 4559382, 4559382, 1982340, 1982340, 1982340, 1982340, 1982340, 1982340, 1982340, 1982340, 1982340, 1982340, 1982340, 1982340, 1982340, 1982340, 1982340, 1982340, 1982340, 1982340, 1982340, 1982340, 1982340, 1982340, 1982340, 1982340, 1982340);
INSERT INTO `baseexp` VALUES (76, 2230113, 2230113, 2230113, 2230113, 2230113, 2230113, 2230113, 2230113, 2230113, 2230113, 2230113, 2230113, 2230113, 2230113, 2230113, 2230113, 2230113, 2230113, 2230113, 2230113, 2230113, 5129260, 5129260, 5129260, 5129260, 5129260, 5129260, 5129260, 5129260, 5129260, 5129260, 5129260, 5129260, 5129260, 5129260, 5129260, 5129260, 5129260, 5129260, 5129260, 5129260, 5129260, 2230113, 2230113, 2230113, 2230113, 2230113, 2230113, 2230113, 2230113, 2230113, 2230113, 2230113, 2230113, 2230113, 2230113, 2230113, 2230113, 2230113, 2230113, 2230113, 2230113, 2230113, 2230113, 2230113, 2230113, 2230113);
INSERT INTO `baseexp` VALUES (77, 2386162, 2386162, 2386162, 2386162, 2386162, 2386162, 2386162, 2386162, 2386162, 2386162, 2386162, 2386162, 2386162, 2386162, 2386162, 2386162, 2386162, 2386162, 2386162, 2386162, 2386162, 5488173, 5488173, 5488173, 5488173, 5488173, 5488173, 5488173, 5488173, 5488173, 5488173, 5488173, 5488173, 5488173, 5488173, 5488173, 5488173, 5488173, 5488173, 5488173, 5488173, 5488173, 2386162, 2386162, 2386162, 2386162, 2386162, 2386162, 2386162, 2386162, 2386162, 2386162, 2386162, 2386162, 2386162, 2386162, 2386162, 2386162, 2386162, 2386162, 2386162, 2386162, 2386162, 2386162, 2386162, 2386162, 2386162);
INSERT INTO `baseexp` VALUES (78, 2547417, 2547417, 2547417, 2547417, 2547417, 2547417, 2547417, 2547417, 2547417, 2547417, 2547417, 2547417, 2547417, 2547417, 2547417, 2547417, 2547417, 2547417, 2547417, 2547417, 2547417, 5859059, 5859059, 5859059, 5859059, 5859059, 5859059, 5859059, 5859059, 5859059, 5859059, 5859059, 5859059, 5859059, 5859059, 5859059, 5859059, 5859059, 5859059, 5859059, 5859059, 5859059, 2547417, 2547417, 2547417, 2547417, 2547417, 2547417, 2547417, 2547417, 2547417, 2547417, 2547417, 2547417, 2547417, 2547417, 2547417, 2547417, 2547417, 2547417, 2547417, 2547417, 2547417, 2547417, 2547417, 2547417, 2547417);
INSERT INTO `baseexp` VALUES (79, 2713878, 2713878, 2713878, 2713878, 2713878, 2713878, 2713878, 2713878, 2713878, 2713878, 2713878, 2713878, 2713878, 2713878, 2713878, 2713878, 2713878, 2713878, 2713878, 2713878, 2713878, 6241919, 6241919, 6241919, 6241919, 6241919, 6241919, 6241919, 6241919, 6241919, 6241919, 6241919, 6241919, 6241919, 6241919, 6241919, 6241919, 6241919, 6241919, 6241919, 6241919, 6241919, 2713878, 2713878, 2713878, 2713878, 2713878, 2713878, 2713878, 2713878, 2713878, 2713878, 2713878, 2713878, 2713878, 2713878, 2713878, 2713878, 2713878, 2713878, 2713878, 2713878, 2713878, 2713878, 2713878, 2713878, 2713878);
INSERT INTO `baseexp` VALUES (80, 3206160, 3206160, 3206160, 3206160, 3206160, 3206160, 3206160, 3206160, 3206160, 3206160, 3206160, 3206160, 3206160, 3206160, 3206160, 3206160, 3206160, 3206160, 3206160, 3206160, 3206160, 7374168, 7374168, 7374168, 7374168, 7374168, 7374168, 7374168, 7374168, 7374168, 7374168, 7374168, 7374168, 7374168, 7374168, 7374168, 7374168, 7374168, 7374168, 7374168, 7374168, 7374168, 3206160, 3206160, 3206160, 3206160, 3206160, 3206160, 3206160, 3206160, 3206160, 3206160, 3206160, 3206160, 3206160, 3206160, 3206160, 3206160, 3206160, 3206160, 3206160, 3206160, 3206160, 3206160, 3206160, 3206160, 3206160);
INSERT INTO `baseexp` VALUES (81, 3681024, 3681024, 3681024, 3681024, 3681024, 3681024, 3681024, 3681024, 3681024, 3681024, 3681024, 3681024, 3681024, 3681024, 3681024, 3681024, 3681024, 3681024, 3681024, 3681024, 3681024, 9570662, 9570662, 9570662, 9570662, 9570662, 9570662, 9570662, 9570662, 9570662, 9570662, 9570662, 9570662, 9570662, 9570662, 9570662, 9570662, 9570662, 9570662, 9570662, 9570662, 9570662, 3681024, 3681024, 3681024, 3681024, 3681024, 3681024, 3681024, 3681024, 3681024, 3681024, 3681024, 3681024, 3681024, 3681024, 3681024, 3681024, 3681024, 3681024, 3681024, 3681024, 3681024, 3681024, 3681024, 3681024, 3681024);
INSERT INTO `baseexp` VALUES (82, 4022472, 4022472, 4022472, 4022472, 4022472, 4022472, 4022472, 4022472, 4022472, 4022472, 4022472, 4022472, 4022472, 4022472, 4022472, 4022472, 4022472, 4022472, 4022472, 4022472, 4022472, 10458427, 10458427, 10458427, 10458427, 10458427, 10458427, 10458427, 10458427, 10458427, 10458427, 10458427, 10458427, 10458427, 10458427, 10458427, 10458427, 10458427, 10458427, 10458427, 10458427, 10458427, 4022472, 4022472, 4022472, 4022472, 4022472, 4022472, 4022472, 4022472, 4022472, 4022472, 4022472, 4022472, 4022472, 4022472, 4022472, 4022472, 4022472, 4022472, 4022472, 4022472, 4022472, 4022472, 4022472, 4022472, 4022472);
INSERT INTO `baseexp` VALUES (83, 4377024, 4377024, 4377024, 4377024, 4377024, 4377024, 4377024, 4377024, 4377024, 4377024, 4377024, 4377024, 4377024, 4377024, 4377024, 4377024, 4377024, 4377024, 4377024, 4377024, 4377024, 11380262, 11380262, 11380262, 11380262, 11380262, 11380262, 11380262, 11380262, 11380262, 11380262, 11380262, 11380262, 11380262, 11380262, 11380262, 11380262, 11380262, 11380262, 11380262, 11380262, 11380262, 4377024, 4377024, 4377024, 4377024, 4377024, 4377024, 4377024, 4377024, 4377024, 4377024, 4377024, 4377024, 4377024, 4377024, 4377024, 4377024, 4377024, 4377024, 4377024, 4377024, 4377024, 4377024, 4377024, 4377024, 4377024);
INSERT INTO `baseexp` VALUES (84, 4744680, 4744680, 4744680, 4744680, 4744680, 4744680, 4744680, 4744680, 4744680, 4744680, 4744680, 4744680, 4744680, 4744680, 4744680, 4744680, 4744680, 4744680, 4744680, 4744680, 4744680, 12336168, 12336168, 12336168, 12336168, 12336168, 12336168, 12336168, 12336168, 12336168, 12336168, 12336168, 12336168, 12336168, 12336168, 12336168, 12336168, 12336168, 12336168, 12336168, 12336168, 12336168, 4744680, 4744680, 4744680, 4744680, 4744680, 4744680, 4744680, 4744680, 4744680, 4744680, 4744680, 4744680, 4744680, 4744680, 4744680, 4744680, 4744680, 4744680, 4744680, 4744680, 4744680, 4744680, 4744680, 4744680, 4744680);
INSERT INTO `baseexp` VALUES (85, 5125440, 5125440, 5125440, 5125440, 5125440, 5125440, 5125440, 5125440, 5125440, 5125440, 5125440, 5125440, 5125440, 5125440, 5125440, 5125440, 5125440, 5125440, 5125440, 5125440, 5125440, 13326144, 13326144, 13326144, 13326144, 13326144, 13326144, 13326144, 13326144, 13326144, 13326144, 13326144, 13326144, 13326144, 13326144, 13326144, 13326144, 13326144, 13326144, 13326144, 13326144, 13326144, 5125440, 5125440, 5125440, 5125440, 5125440, 5125440, 5125440, 5125440, 5125440, 5125440, 5125440, 5125440, 5125440, 5125440, 5125440, 5125440, 5125440, 5125440, 5125440, 5125440, 5125440, 5125440, 5125440, 5125440, 5125440);
INSERT INTO `baseexp` VALUES (86, 5767272, 5767272, 5767272, 5767272, 5767272, 5767272, 5767272, 5767272, 5767272, 5767272, 5767272, 5767272, 5767272, 5767272, 5767272, 5767272, 5767272, 5767272, 5767272, 5767272, 5767272, 14994907, 14994907, 14994907, 14994907, 14994907, 14994907, 14994907, 14994907, 14994907, 14994907, 14994907, 14994907, 14994907, 14994907, 14994907, 14994907, 14994907, 14994907, 14994907, 14994907, 14994907, 5767272, 5767272, 5767272, 5767272, 5767272, 5767272, 5767272, 5767272, 5767272, 5767272, 5767272, 5767272, 5767272, 5767272, 5767272, 5767272, 5767272, 5767272, 5767272, 5767272, 5767272, 5767272, 5767272, 5767272, 5767272);
INSERT INTO `baseexp` VALUES (87, 6204000, 6204000, 6204000, 6204000, 6204000, 6204000, 6204000, 6204000, 6204000, 6204000, 6204000, 6204000, 6204000, 6204000, 6204000, 6204000, 6204000, 6204000, 6204000, 6204000, 6204000, 16130400, 16130400, 16130400, 16130400, 16130400, 16130400, 16130400, 16130400, 16130400, 16130400, 16130400, 16130400, 16130400, 16130400, 16130400, 16130400, 16130400, 16130400, 16130400, 16130400, 16130400, 6204000, 6204000, 6204000, 6204000, 6204000, 6204000, 6204000, 6204000, 6204000, 6204000, 6204000, 6204000, 6204000, 6204000, 6204000, 6204000, 6204000, 6204000, 6204000, 6204000, 6204000, 6204000, 6204000, 6204000, 6204000);
INSERT INTO `baseexp` VALUES (88, 6655464, 6655464, 6655464, 6655464, 6655464, 6655464, 6655464, 6655464, 6655464, 6655464, 6655464, 6655464, 6655464, 6655464, 6655464, 6655464, 6655464, 6655464, 6655464, 6655464, 6655464, 17304206, 17304206, 17304206, 17304206, 17304206, 17304206, 17304206, 17304206, 17304206, 17304206, 17304206, 17304206, 17304206, 17304206, 17304206, 17304206, 17304206, 17304206, 17304206, 17304206, 17304206, 6655464, 6655464, 6655464, 6655464, 6655464, 6655464, 6655464, 6655464, 6655464, 6655464, 6655464, 6655464, 6655464, 6655464, 6655464, 6655464, 6655464, 6655464, 6655464, 6655464, 6655464, 6655464, 6655464, 6655464, 6655464);
INSERT INTO `baseexp` VALUES (89, 7121664, 7121664, 7121664, 7121664, 7121664, 7121664, 7121664, 7121664, 7121664, 7121664, 7121664, 7121664, 7121664, 7121664, 7121664, 7121664, 7121664, 7121664, 7121664, 7121664, 7121664, 18516326, 18516326, 18516326, 18516326, 18516326, 18516326, 18516326, 18516326, 18516326, 18516326, 18516326, 18516326, 18516326, 18516326, 18516326, 18516326, 18516326, 18516326, 18516326, 18516326, 18516326, 7121664, 7121664, 7121664, 7121664, 7121664, 7121664, 7121664, 7121664, 7121664, 7121664, 7121664, 7121664, 7121664, 7121664, 7121664, 7121664, 7121664, 7121664, 7121664, 7121664, 7121664, 7121664, 7121664, 7121664, 7121664);
INSERT INTO `baseexp` VALUES (90, 7602600, 7602600, 7602600, 7602600, 7602600, 7602600, 7602600, 7602600, 7602600, 7602600, 7602600, 7602600, 7602600, 7602600, 7602600, 7602600, 7602600, 7602600, 7602600, 7602600, 7602600, 19766760, 19766760, 19766760, 19766760, 19766760, 19766760, 19766760, 19766760, 19766760, 19766760, 19766760, 19766760, 19766760, 19766760, 19766760, 19766760, 19766760, 19766760, 19766760, 19766760, 19766760, 7602600, 7602600, 7602600, 7602600, 7602600, 7602600, 7602600, 7602600, 7602600, 7602600, 7602600, 7602600, 7602600, 7602600, 7602600, 7602600, 7602600, 7602600, 7602600, 7602600, 7602600, 7602600, 7602600, 7602600, 7602600);
INSERT INTO `baseexp` VALUES (91, 9738720, 9738720, 9738720, 9738720, 9738720, 9738720, 9738720, 9738720, 9738720, 9738720, 9738720, 9738720, 9738720, 9738720, 9738720, 9738720, 9738720, 9738720, 9738720, 9738720, 9738720, 29216160, 29216160, 29216160, 29216160, 29216160, 29216160, 29216160, 29216160, 29216160, 29216160, 29216160, 29216160, 29216160, 29216160, 29216160, 29216160, 29216160, 29216160, 29216160, 29216160, 29216160, 9738720, 9738720, 9738720, 9738720, 9738720, 9738720, 9738720, 9738720, 9738720, 9738720, 9738720, 9738720, 9738720, 9738720, 9738720, 9738720, 9738720, 9738720, 9738720, 9738720, 9738720, 9738720, 9738720, 9738720, 9738720);
INSERT INTO `baseexp` VALUES (92, 11649960, 11649960, 11649960, 11649960, 11649960, 11649960, 11649960, 11649960, 11649960, 11649960, 11649960, 11649960, 11649960, 11649960, 11649960, 11649960, 11649960, 11649960, 11649960, 11649960, 11649960, 34949880, 34949880, 34949880, 34949880, 34949880, 34949880, 34949880, 34949880, 34949880, 34949880, 34949880, 34949880, 34949880, 34949880, 34949880, 34949880, 34949880, 34949880, 34949880, 34949880, 34949880, 11649960, 11649960, 11649960, 11649960, 11649960, 11649960, 11649960, 11649960, 11649960, 11649960, 11649960, 11649960, 11649960, 11649960, 11649960, 11649960, 11649960, 11649960, 11649960, 11649960, 11649960, 11649960, 11649960, 11649960, 11649960);
INSERT INTO `baseexp` VALUES (93, 13643520, 13643520, 13643520, 13643520, 13643520, 13643520, 13643520, 13643520, 13643520, 13643520, 13643520, 13643520, 13643520, 13643520, 13643520, 13643520, 13643520, 13643520, 13643520, 13643520, 13643520, 40930560, 40930560, 40930560, 40930560, 40930560, 40930560, 40930560, 40930560, 40930560, 40930560, 40930560, 40930560, 40930560, 40930560, 40930560, 40930560, 40930560, 40930560, 40930560, 40930560, 40930560, 13643520, 13643520, 13643520, 13643520, 13643520, 13643520, 13643520, 13643520, 13643520, 13643520, 13643520, 13643520, 13643520, 13643520, 13643520, 13643520, 13643520, 13643520, 13643520, 13643520, 13643520, 13643520, 13643520, 13643520, 13643520);
INSERT INTO `baseexp` VALUES (94, 18339300, 18339300, 18339300, 18339300, 18339300, 18339300, 18339300, 18339300, 18339300, 18339300, 18339300, 18339300, 18339300, 18339300, 18339300, 18339300, 18339300, 18339300, 18339300, 18339300, 18339300, 55017900, 55017900, 55017900, 55017900, 55017900, 55017900, 55017900, 55017900, 55017900, 55017900, 55017900, 55017900, 55017900, 55017900, 55017900, 55017900, 55017900, 55017900, 55017900, 55017900, 55017900, 18339300, 18339300, 18339300, 18339300, 18339300, 18339300, 18339300, 18339300, 18339300, 18339300, 18339300, 18339300, 18339300, 18339300, 18339300, 18339300, 18339300, 18339300, 18339300, 18339300, 18339300, 18339300, 18339300, 18339300, 18339300);
INSERT INTO `baseexp` VALUES (95, 23836800, 23836800, 23836800, 23836800, 23836800, 23836800, 23836800, 23836800, 23836800, 23836800, 23836800, 23836800, 23836800, 23836800, 23836800, 23836800, 23836800, 23836800, 23836800, 23836800, 23836800, 71510400, 71510400, 71510400, 71510400, 71510400, 71510400, 71510400, 71510400, 71510400, 71510400, 71510400, 71510400, 71510400, 71510400, 71510400, 71510400, 71510400, 71510400, 71510400, 71510400, 71510400, 23836800, 23836800, 23836800, 23836800, 23836800, 23836800, 23836800, 23836800, 23836800, 23836800, 23836800, 23836800, 23836800, 23836800, 23836800, 23836800, 23836800, 23836800, 23836800, 23836800, 23836800, 23836800, 23836800, 23836800, 23836800);
INSERT INTO `baseexp` VALUES (96, 35658000, 35658000, 35658000, 35658000, 35658000, 35658000, 35658000, 35658000, 35658000, 35658000, 35658000, 35658000, 35658000, 35658000, 35658000, 35658000, 35658000, 35658000, 35658000, 35658000, 35658000, 106974000, 106974000, 106974000, 106974000, 106974000, 106974000, 106974000, 106974000, 106974000, 106974000, 106974000, 106974000, 106974000, 106974000, 106974000, 106974000, 106974000, 106974000, 106974000, 106974000, 106974000, 35658000, 35658000, 35658000, 35658000, 35658000, 35658000, 35658000, 35658000, 35658000, 35658000, 35658000, 35658000, 35658000, 35658000, 35658000, 35658000, 35658000, 35658000, 35658000, 35658000, 35658000, 35658000, 35658000, 35658000, 35658000);
INSERT INTO `baseexp` VALUES (97, 48687000, 48687000, 48687000, 48687000, 48687000, 48687000, 48687000, 48687000, 48687000, 48687000, 48687000, 48687000, 48687000, 48687000, 48687000, 48687000, 48687000, 48687000, 48687000, 48687000, 48687000, 146061000, 146061000, 146061000, 146061000, 146061000, 146061000, 146061000, 146061000, 146061000, 146061000, 146061000, 146061000, 146061000, 146061000, 146061000, 146061000, 146061000, 146061000, 146061000, 146061000, 146061000, 48687000, 48687000, 48687000, 48687000, 48687000, 48687000, 48687000, 48687000, 48687000, 48687000, 48687000, 48687000, 48687000, 48687000, 48687000, 48687000, 48687000, 48687000, 48687000, 48687000, 48687000, 48687000, 48687000, 48687000, 48687000);
INSERT INTO `baseexp` VALUES (98, 58135000, 58135000, 58135000, 58135000, 58135000, 58135000, 58135000, 58135000, 58135000, 58135000, 58135000, 58135000, 58135000, 58135000, 58135000, 58135000, 58135000, 58135000, 58135000, 58135000, 58135000, 174405000, 174405000, 174405000, 174405000, 174405000, 174405000, 174405000, 174405000, 174405000, 174405000, 174405000, 174405000, 174405000, 174405000, 174405000, 174405000, 174405000, 174405000, 174405000, 174405000, 174405000, 58135000, 58135000, 58135000, 58135000, 58135000, 58135000, 58135000, 58135000, 58135000, 58135000, 58135000, 58135000, 58135000, 58135000, 58135000, 58135000, 58135000, 58135000, 58135000, 58135000, 58135000, 58135000, 58135000, 58135000, 58135000);
INSERT INTO `baseexp` VALUES (99, 99999998, 99999998, 99999998, 99999998, 99999998, 99999998, 99999998, 99999998, 99999998, 99999998, 99999998, 99999998, 99999998, 99999998, 99999998, 99999998, 99999998, 99999998, 99999998, 99999998, 99999998, 343210000, 343210000, 343210000, 343210000, 343210000, 343210000, 343210000, 343210000, 343210000, 343210000, 343210000, 343210000, 343210000, 343210000, 343210000, 343210000, 343210000, 343210000, 343210000, 343210000, 343210000, 99999998, 99999998, 99999998, 99999998, 99999998, 99999998, 99999998, 99999998, 99999998, 99999998, 99999998, 99999998, 99999998, 99999998, 99999998, 99999998, 99999998, 99999998, 99999998, 99999998, 99999998, 99999998, 99999998, 99999998, 99999998);
INSERT INTO `jobexp` VALUES (1, 4, 12, 12, 12, 12, 12, 12, 107, 107, 107, 107, 107, 107, 107, 107, 107, 107, 107, 107, 107, 12, 4, 24, 24, 24, 24, 24, 24, 214, 214, 214, 214, 214, 214, 214, 214, 214, 214, 214, 214, 214, 4, 12, 12, 12, 12, 12, 12, 107, 107, 107, 107, 107, 107, 107, 107, 107, 107, 107, 107, 107, 107, 107, 107, 107, 107, 107);
INSERT INTO `jobexp` VALUES (2, 10, 30, 30, 30, 30, 30, 30, 144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 30, 11, 60, 60, 60, 60, 60, 60, 288, 288, 288, 288, 288, 288, 288, 288, 288, 288, 288, 288, 288, 10, 30, 30, 30, 30, 30, 30, 144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 144);
INSERT INTO `jobexp` VALUES (3, 18, 43, 43, 43, 43, 43, 43, 184, 184, 184, 184, 184, 184, 184, 184, 184, 184, 184, 184, 184, 43, 20, 86, 86, 86, 86, 86, 86, 368, 368, 368, 368, 368, 368, 368, 368, 368, 368, 368, 368, 368, 18, 43, 43, 43, 43, 43, 43, 184, 184, 184, 184, 184, 184, 184, 184, 184, 184, 184, 184, 184, 184, 184, 184, 184, 184, 184);
INSERT INTO `jobexp` VALUES (4, 28, 58, 58, 58, 58, 58, 58, 284, 284, 284, 284, 284, 284, 284, 284, 284, 284, 284, 284, 284, 58, 31, 116, 116, 116, 116, 116, 116, 568, 568, 568, 568, 568, 568, 568, 568, 568, 568, 568, 568, 568, 28, 58, 58, 58, 58, 58, 58, 284, 284, 284, 284, 284, 284, 284, 284, 284, 284, 284, 284, 284, 284, 284, 284, 284, 284, 284);
INSERT INTO `jobexp` VALUES (5, 40, 76, 76, 76, 76, 76, 76, 348, 348, 348, 348, 348, 348, 348, 348, 348, 348, 348, 348, 348, 76, 44, 152, 152, 152, 152, 152, 152, 696, 696, 696, 696, 696, 696, 696, 696, 696, 696, 696, 696, 696, 40, 76, 76, 76, 76, 76, 76, 348, 348, 348, 348, 348, 348, 348, 348, 348, 348, 348, 348, 348, 348, 348, 348, 348, 348, 348);
INSERT INTO `jobexp` VALUES (6, 91, 116, 116, 116, 116, 116, 116, 603, 603, 603, 603, 603, 603, 603, 603, 603, 603, 603, 603, 603, 116, 100, 232, 232, 232, 232, 232, 232, 1206, 1206, 1206, 1206, 1206, 1206, 1206, 1206, 1206, 1206, 1206, 1206, 1206, 91, 116, 116, 116, 116, 116, 116, 603, 603, 603, 603, 603, 603, 603, 603, 603, 603, 603, 603, 603, 603, 603, 603, 603, 603, 603);
INSERT INTO `jobexp` VALUES (7, 151, 180, 180, 180, 180, 180, 180, 887, 887, 887, 887, 887, 887, 887, 887, 887, 887, 887, 887, 887, 180, 166, 360, 360, 360, 360, 360, 360, 1774, 1774, 1774, 1774, 1774, 1774, 1774, 1774, 1774, 1774, 1774, 1774, 1774, 151, 180, 180, 180, 180, 180, 180, 887, 887, 887, 887, 887, 887, 887, 887, 887, 887, 887, 887, 887, 887, 887, 887, 887, 887, 887);
INSERT INTO `jobexp` VALUES (8, 205, 220, 220, 220, 220, 220, 220, 1096, 1096, 1096, 1096, 1096, 1096, 1096, 1096, 1096, 1096, 1096, 1096, 1096, 220, 226, 440, 440, 440, 440, 440, 440, 2192, 2192, 2192, 2192, 2192, 2192, 2192, 2192, 2192, 2192, 2192, 2192, 2192, 205, 220, 220, 220, 220, 220, 220, 1096, 1096, 1096, 1096, 1096, 1096, 1096, 1096, 1096, 1096, 1096, 1096, 1096, 1096, 1096, 1096, 1096, 1096, 1096);
INSERT INTO `jobexp` VALUES (9, 268, 272, 272, 272, 272, 272, 272, 1598, 1598, 1598, 1598, 1598, 1598, 1598, 1598, 1598, 1598, 1598, 1598, 1598, 272, 295, 544, 544, 544, 544, 544, 544, 3196, 3196, 3196, 3196, 3196, 3196, 3196, 3196, 3196, 3196, 3196, 3196, 3196, 268, 272, 272, 272, 272, 272, 272, 1598, 1598, 1598, 1598, 1598, 1598, 1598, 1598, 1598, 1598, 1598, 1598, 1598, 1598, 1598, 1598, 1598, 1598, 1598);
INSERT INTO `jobexp` VALUES (10, 340, 336, 336, 336, 336, 336, 336, 2540, 2540, 2540, 2540, 2540, 2540, 2540, 2540, 2540, 2540, 2540, 2540, 2540, 336, 374, 672, 672, 672, 672, 672, 672, 5080, 5080, 5080, 5080, 5080, 5080, 5080, 5080, 5080, 5080, 5080, 5080, 5080, 340, 336, 336, 336, 336, 336, 336, 2540, 2540, 2540, 2540, 2540, 2540, 2540, 2540, 2540, 2540, 2540, 2540, 2540, 2540, 2540, 2540, 2540, 2540, 2540);
INSERT INTO `jobexp` VALUES (11, NULL, 520, 520, 520, 520, 520, 520, 3676, 3676, 3676, 3676, 3676, 3676, 3676, 3676, 3676, 3676, 3676, 3676, 3676, 520, NULL, 1040, 1040, 1040, 1040, 1040, 1040, 7352, 7352, 7352, 7352, 7352, 7352, 7352, 7352, 7352, 7352, 7352, 7352, 7352, NULL, 520, 520, 520, 520, 520, 520, 3676, 3676, 3676, 3676, 3676, 3676, 3676, 3676, 3676, 3676, 3676, 3676, 3676, 3676, 3676, 3676, 3676, 3676, 3676);
INSERT INTO `jobexp` VALUES (12, NULL, 604, 604, 604, 604, 604, 604, 4290, 4290, 4290, 4290, 4290, 4290, 4290, 4290, 4290, 4290, 4290, 4290, 4290, 604, NULL, 1208, 1208, 1208, 1208, 1208, 1208, 8580, 8580, 8580, 8580, 8580, 8580, 8580, 8580, 8580, 8580, 8580, 8580, 8580, NULL, 604, 604, 604, 604, 604, 604, 4290, 4290, 4290, 4290, 4290, 4290, 4290, 4290, 4290, 4290, 4290, 4290, 4290, 4290, 4290, 4290, 4290, 4290, 4290);
INSERT INTO `jobexp` VALUES (13, NULL, 699, 699, 699, 699, 699, 699, 4946, 4946, 4946, 4946, 4946, 4946, 4946, 4946, 4946, 4946, 4946, 4946, 4946, 699, NULL, 1398, 1398, 1398, 1398, 1398, 1398, 9892, 9892, 9892, 9892, 9892, 9892, 9892, 9892, 9892, 9892, 9892, 9892, 9892, NULL, 699, 699, 699, 699, 699, 699, 4946, 4946, 4946, 4946, 4946, 4946, 4946, 4946, 4946, 4946, 4946, 4946, 4946, 4946, 4946, 4946, 4946, 4946, 4946);
INSERT INTO `jobexp` VALUES (14, NULL, 802, 802, 802, 802, 802, 802, 6679, 6679, 6679, 6679, 6679, 6679, 6679, 6679, 6679, 6679, 6679, 6679, 6679, 802, NULL, 1604, 1604, 1604, 1604, 1604, 1604, 13358, 13358, 13358, 13358, 13358, 13358, 13358, 13358, 13358, 13358, 13358, 13358, 13358, NULL, 802, 802, 802, 802, 802, 802, 6679, 6679, 6679, 6679, 6679, 6679, 6679, 6679, 6679, 6679, 6679, 6679, 6679, 6679, 6679, 6679, 6679, 6679, 6679);
INSERT INTO `jobexp` VALUES (15, NULL, 948, 948, 948, 948, 948, 948, 9492, 9492, 9492, 9492, 9492, 9492, 9492, 9492, 9492, 9492, 9492, 9492, 9492, 948, NULL, 1896, 1896, 1896, 1896, 1896, 1896, 18984, 18984, 18984, 18984, 18984, 18984, 18984, 18984, 18984, 18984, 18984, 18984, 18984, NULL, 948, 948, 948, 948, 948, 948, 9492, 9492, 9492, 9492, 9492, 9492, 9492, 9492, 9492, 9492, 9492, 9492, 9492, 9492, 9492, 9492, 9492, 9492, 9492);
INSERT INTO `jobexp` VALUES (16, NULL, 1125, 1125, 1125, 1125, 1125, 1125, 12770, 12770, 12770, 12770, 12770, 12770, 12770, 12770, 12770, 12770, 12770, 12770, 12770, 1125, NULL, 2250, 2250, 2250, 2250, 2250, 2250, 31925, 31925, 31925, 31925, 31925, 31925, 31925, 31925, 31925, 31925, 31925, 31925, 31925, NULL, 1125, 1125, 1125, 1125, 1125, 1125, 12770, 12770, 12770, 12770, 12770, 12770, 12770, 12770, 12770, 12770, 12770, 12770, 12770, 12770, 12770, 12770, 12770, 12770, 12770);
INSERT INTO `jobexp` VALUES (17, NULL, 1668, 1668, 1668, 1668, 1668, 1668, 14344, 14344, 14344, 14344, 14344, 14344, 14344, 14344, 14344, 14344, 14344, 14344, 14344, 1668, NULL, 3336, 3336, 3336, 3336, 3336, 3336, 35860, 35860, 35860, 35860, 35860, 35860, 35860, 35860, 35860, 35860, 35860, 35860, 35860, NULL, 1668, 1668, 1668, 1668, 1668, 1668, 14344, 14344, 14344, 14344, 14344, 14344, 14344, 14344, 14344, 14344, 14344, 14344, 14344, 14344, 14344, 14344, 14344, 14344, 14344);
INSERT INTO `jobexp` VALUES (18, NULL, 1937, 1937, 1937, 1937, 1937, 1937, 16005, 16005, 16005, 16005, 16005, 16005, 16005, 16005, 16005, 16005, 16005, 16005, 16005, 1937, NULL, 3874, 3874, 3874, 3874, 3874, 3874, 40013, 40013, 40013, 40013, 40013, 40013, 40013, 40013, 40013, 40013, 40013, 40013, 40013, NULL, 1937, 1937, 1937, 1937, 1937, 1937, 16005, 16005, 16005, 16005, 16005, 16005, 16005, 16005, 16005, 16005, 16005, 16005, 16005, 16005, 16005, 16005, 16005, 16005, 16005);
INSERT INTO `jobexp` VALUES (19, NULL, 2226, 2226, 2226, 2226, 2226, 2226, 20642, 20642, 20642, 20642, 20642, 20642, 20642, 20642, 20642, 20642, 20642, 20642, 20642, 2226, NULL, 4452, 4452, 4452, 4452, 4452, 4452, 51605, 51605, 51605, 51605, 51605, 51605, 51605, 51605, 51605, 51605, 51605, 51605, 51605, NULL, 2226, 2226, 2226, 2226, 2226, 2226, 20642, 20642, 20642, 20642, 20642, 20642, 20642, 20642, 20642, 20642, 20642, 20642, 20642, 20642, 20642, 20642, 20642, 20642, 20642);
INSERT INTO `jobexp` VALUES (20, NULL, 3040, 3040, 3040, 3040, 3040, 3040, 27434, 27434, 27434, 27434, 27434, 27434, 27434, 27434, 27434, 27434, 27434, 27434, 27434, 3040, NULL, 6080, 6080, 6080, 6080, 6080, 6080, 68585, 68585, 68585, 68585, 68585, 68585, 68585, 68585, 68585, 68585, 68585, 68585, 68585, NULL, 3040, 3040, 3040, 3040, 3040, 3040, 27434, 27434, 27434, 27434, 27434, 27434, 27434, 27434, 27434, 27434, 27434, 27434, 27434, 27434, 27434, 27434, 27434, 27434, 27434);
INSERT INTO `jobexp` VALUES (21, NULL, 3988, 3988, 3988, 3988, 3988, 3988, 35108, 35108, 35108, 35108, 35108, 35108, 35108, 35108, 35108, 35108, 35108, 35108, 35108, 3988, NULL, 7976, 7976, 7976, 7976, 7976, 7976, 87770, 87770, 87770, 87770, 87770, 87770, 87770, 87770, 87770, 87770, 87770, 87770, 87770, NULL, 3988, 3988, 3988, 3988, 3988, 3988, 35108, 35108, 35108, 35108, 35108, 35108, 35108, 35108, 35108, 35108, 35108, 35108, 35108, 35108, 35108, 35108, 35108, 35108, 35108);
INSERT INTO `jobexp` VALUES (22, NULL, 5564, 5564, 5564, 5564, 5564, 5564, 38577, 38577, 38577, 38577, 38577, 38577, 38577, 38577, 38577, 38577, 38577, 38577, 38577, 5564, NULL, 11128, 11128, 11128, 11128, 11128, 11128, 96443, 96443, 96443, 96443, 96443, 96443, 96443, 96443, 96443, 96443, 96443, 96443, 96443, NULL, 5564, 5564, 5564, 5564, 5564, 5564, 38577, 38577, 38577, 38577, 38577, 38577, 38577, 38577, 38577, 38577, 38577, 38577, 38577, 38577, 38577, 38577, 38577, 38577, 38577);
INSERT INTO `jobexp` VALUES (23, NULL, 6272, 6272, 6272, 6272, 6272, 6272, 42206, 42206, 42206, 42206, 42206, 42206, 42206, 42206, 42206, 42206, 42206, 42206, 42206, 6272, NULL, 12544, 12544, 12544, 12544, 12544, 12544, 105515, 105515, 105515, 105515, 105515, 105515, 105515, 105515, 105515, 105515, 105515, 105515, 105515, NULL, 6272, 6272, 6272, 6272, 6272, 6272, 42206, 42206, 42206, 42206, 42206, 42206, 42206, 42206, 42206, 42206, 42206, 42206, 42206, 42206, 42206, 42206, 42206, 42206, 42206);
INSERT INTO `jobexp` VALUES (24, NULL, 7021, 7021, 7021, 7021, 7021, 7021, 52708, 52708, 52708, 52708, 52708, 52708, 52708, 52708, 52708, 52708, 52708, 52708, 52708, 7021, NULL, 14042, 14042, 14042, 14042, 14042, 14042, 131770, 131770, 131770, 131770, 131770, 131770, 131770, 131770, 131770, 131770, 131770, 131770, 131770, NULL, 7021, 7021, 7021, 7021, 7021, 7021, 52708, 52708, 52708, 52708, 52708, 52708, 52708, 52708, 52708, 52708, 52708, 52708, 52708, 52708, 52708, 52708, 52708, 52708, 52708);
INSERT INTO `jobexp` VALUES (25, NULL, 9114, 9114, 9114, 9114, 9114, 9114, 66971, 66971, 66971, 66971, 66971, 66971, 66971, 66971, 66971, 66971, 66971, 66971, 66971, 9114, NULL, 18228, 18228, 18228, 18228, 18228, 18228, 167428, 167428, 167428, 167428, 167428, 167428, 167428, 167428, 167428, 167428, 167428, 167428, 167428, NULL, 9114, 9114, 9114, 9114, 9114, 9114, 66971, 66971, 66971, 66971, 66971, 66971, 66971, 66971, 66971, 66971, 66971, 66971, 66971, 66971, 66971, 66971, 66971, 66971, 66971);
INSERT INTO `jobexp` VALUES (26, NULL, 11473, 11473, 11473, 11473, 11473, 11473, 82688, 82688, 82688, 82688, 82688, 82688, 82688, 82688, 82688, 82688, 82688, 82688, 82688, 11473, NULL, 28683, 28683, 28683, 28683, 28683, 28683, 206720, 206720, 206720, 206720, 206720, 206720, 206720, 206720, 206720, 206720, 206720, 206720, 206720, NULL, 11473, 11473, 11473, 11473, 11473, 11473, 82688, 82688, 82688, 82688, 82688, 82688, 82688, 82688, 82688, 82688, 82688, 82688, 82688, 82688, 82688, 82688, 82688, 82688, 82688);
INSERT INTO `jobexp` VALUES (27, NULL, 15290, 15290, 15290, 15290, 15290, 15290, 89544, 89544, 89544, 89544, 89544, 89544, 89544, 89544, 89544, 89544, 89544, 89544, 89544, 15290, NULL, 38225, 38225, 38225, 38225, 38225, 38225, 223860, 223860, 223860, 223860, 223860, 223860, 223860, 223860, 223860, 223860, 223860, 223860, 223860, NULL, 15290, 15290, 15290, 15290, 15290, 15290, 89544, 89544, 89544, 89544, 89544, 89544, 89544, 89544, 89544, 89544, 89544, 89544, 89544, 89544, 89544, 89544, 89544, 89544, 89544);
INSERT INTO `jobexp` VALUES (28, NULL, 16891, 16891, 16891, 16891, 16891, 16891, 96669, 96669, 96669, 96669, 96669, 96669, 96669, 96669, 96669, 96669, 96669, 96669, 96669, 16891, NULL, 42228, 42228, 42228, 42228, 42228, 42228, 241673, 241673, 241673, 241673, 241673, 241673, 241673, 241673, 241673, 241673, 241673, 241673, 241673, NULL, 16891, 16891, 16891, 16891, 16891, 16891, 96669, 96669, 96669, 96669, 96669, 96669, 96669, 96669, 96669, 96669, 96669, 96669, 96669, 96669, 96669, 96669, 96669, 96669, 96669);
INSERT INTO `jobexp` VALUES (29, NULL, 18570, 18570, 18570, 18570, 18570, 18570, 117821, 117821, 117821, 117821, 117821, 117821, 117821, 117821, 117821, 117821, 117821, 117821, 117821, 18570, NULL, 46425, 46425, 46425, 46425, 46425, 46425, 294553, 294553, 294553, 294553, 294553, 294553, 294553, 294553, 294553, 294553, 294553, 294553, 294553, NULL, 18570, 18570, 18570, 18570, 18570, 18570, 117821, 117821, 117821, 117821, 117821, 117821, 117821, 117821, 117821, 117821, 117821, 117821, 117821, 117821, 117821, 117821, 117821, 117821, 117821);
INSERT INTO `jobexp` VALUES (30, NULL, 23229, 23229, 23229, 23229, 23229, 23229, 144921, 144921, 144921, 144921, 144921, 144921, 144921, 144921, 144921, 144921, 144921, 144921, 144921, 23229, NULL, 58073, 58073, 58073, 58073, 58073, 58073, 362303, 362303, 362303, 362303, 362303, 362303, 362303, 362303, 362303, 362303, 362303, 362303, 362303, NULL, 23229, 23229, 23229, 23229, 23229, 23229, 144921, 144921, 144921, 144921, 144921, 144921, 144921, 144921, 144921, 144921, 144921, 144921, 144921, 144921, 144921, 144921, 144921, 144921, 144921);
INSERT INTO `jobexp` VALUES (31, NULL, 28359, 28359, 28359, 28359, 28359, 28359, 174201, 174201, 174201, 174201, 174201, 174201, 174201, 174201, 174201, 174201, 174201, 174201, 174201, 28359, NULL, 70898, 70898, 70898, 70898, 70898, 70898, 479053, 479053, 479053, 479053, 479053, 479053, 479053, 479053, 479053, 479053, 479053, 479053, 479053, NULL, 28359, 28359, 28359, 28359, 28359, 28359, 174201, 174201, 174201, 174201, 174201, 174201, 174201, 174201, 174201, 174201, 174201, 174201, 174201, 174201, 174201, 174201, 174201, 174201, 174201);
INSERT INTO `jobexp` VALUES (32, NULL, 36478, 36478, 36478, 36478, 36478, 36478, 186677, 186677, 186677, 186677, 186677, 186677, 186677, 186677, 186677, 186677, 186677, 186677, 186677, 36478, NULL, 91195, 91195, 91195, 91195, 91195, 91195, 513362, 513362, 513362, 513362, 513362, 513362, 513362, 513362, 513362, 513362, 513362, 513362, 513362, NULL, 36478, 36478, 36478, 36478, 36478, 36478, 186677, 186677, 186677, 186677, 186677, 186677, 186677, 186677, 186677, 186677, 186677, 186677, 186677, 186677, 186677, 186677, 186677, 186677, 186677);
INSERT INTO `jobexp` VALUES (33, NULL, 39716, 39716, 39716, 39716, 39716, 39716, 199584, 199584, 199584, 199584, 199584, 199584, 199584, 199584, 199584, 199584, 199584, 199584, 199584, 39716, NULL, 99290, 99290, 99290, 99290, 99290, 99290, 548856, 548856, 548856, 548856, 548856, 548856, 548856, 548856, 548856, 548856, 548856, 548856, 548856, NULL, 39716, 39716, 39716, 39716, 39716, 39716, 199584, 199584, 199584, 199584, 199584, 199584, 199584, 199584, 199584, 199584, 199584, 199584, 199584, 199584, 199584, 199584, 199584, 199584, 199584);
INSERT INTO `jobexp` VALUES (34, NULL, 43088, 43088, 43088, 43088, 43088, 43088, 238617, 238617, 238617, 238617, 238617, 238617, 238617, 238617, 238617, 238617, 238617, 238617, 238617, 43088, NULL, 107720, 107720, 107720, 107720, 107720, 107720, 656197, 656197, 656197, 656197, 656197, 656197, 656197, 656197, 656197, 656197, 656197, 656197, 656197, NULL, 43088, 43088, 43088, 43088, 43088, 43088, 238617, 238617, 238617, 238617, 238617, 238617, 238617, 238617, 238617, 238617, 238617, 238617, 238617, 238617, 238617, 238617, 238617, 238617, 238617);
INSERT INTO `jobexp` VALUES (35, NULL, 52417, 52417, 52417, 52417, 52417, 52417, 286366, 286366, 286366, 286366, 286366, 286366, 286366, 286366, 286366, 286366, 286366, 286366, 286366, 52417, NULL, 131043, 131043, 131043, 131043, 131043, 131043, 787507, 787507, 787507, 787507, 787507, 787507, 787507, 787507, 787507, 787507, 787507, 787507, 787507, NULL, 52417, 52417, 52417, 52417, 52417, 52417, 286366, 286366, 286366, 286366, 286366, 286366, 286366, 286366, 286366, 286366, 286366, 286366, 286366, 286366, 286366, 286366, 286366, 286366, 286366);
INSERT INTO `jobexp` VALUES (36, NULL, 62495, 62495, 62495, 62495, 62495, 62495, 337147, 337147, 337147, 337147, 337147, 337147, 337147, 337147, 337147, 337147, 337147, 337147, 337147, 62495, NULL, 156238, 156238, 156238, 156238, 156238, 156238, 927154, 927154, 927154, 927154, 927154, 927154, 927154, 927154, 927154, 927154, 927154, 927154, 927154, NULL, 62495, 62495, 62495, 62495, 62495, 62495, 337147, 337147, 337147, 337147, 337147, 337147, 337147, 337147, 337147, 337147, 337147, 337147, 337147, 337147, 337147, 337147, 337147, 337147, 337147);
INSERT INTO `jobexp` VALUES (37, NULL, 78160, 78160, 78160, 78160, 78160, 78160, 358435, 358435, 358435, 358435, 358435, 358435, 358435, 358435, 358435, 358435, 358435, 358435, 358435, 78160, NULL, 195400, 195400, 195400, 195400, 195400, 195400, 985696, 985696, 985696, 985696, 985696, 985696, 985696, 985696, 985696, 985696, 985696, 985696, 985696, NULL, 78160, 78160, 78160, 78160, 78160, 78160, 358435, 358435, 358435, 358435, 358435, 358435, 358435, 358435, 358435, 358435, 358435, 358435, 358435, 358435, 358435, 358435, 358435, 358435, 358435);
INSERT INTO `jobexp` VALUES (38, NULL, 84175, 84175, 84175, 84175, 84175, 84175, 380376, 380376, 380376, 380376, 380376, 380376, 380376, 380376, 380376, 380376, 380376, 380376, 380376, 84175, NULL, 210438, 210438, 210438, 210438, 210438, 210438, 1046034, 1046034, 1046034, 1046034, 1046034, 1046034, 1046034, 1046034, 1046034, 1046034, 1046034, 1046034, 1046034, NULL, 84175, 84175, 84175, 84175, 84175, 84175, 380376, 380376, 380376, 380376, 380376, 380376, 380376, 380376, 380376, 380376, 380376, 380376, 380376, 380376, 380376, 380376, 380376, 380376, 380376);
INSERT INTO `jobexp` VALUES (39, NULL, 90404, 90404, 90404, 90404, 90404, 90404, 447685, 447685, 447685, 447685, 447685, 447685, 447685, 447685, 447685, 447685, 447685, 447685, 447685, 90404, NULL, 226010, 226010, 226010, 226010, 226010, 226010, 1231134, 1231134, 1231134, 1231134, 1231134, 1231134, 1231134, 1231134, 1231134, 1231134, 1231134, 1231134, 1231134, NULL, 90404, 90404, 90404, 90404, 90404, 90404, 447685, 447685, 447685, 447685, 447685, 447685, 447685, 447685, 447685, 447685, 447685, 447685, 447685, 447685, 447685, 447685, 447685, 447685, 447685);
INSERT INTO `jobexp` VALUES (40, NULL, 107611, 107611, 107611, 107611, 107611, 107611, 526989, 526989, 526989, 526989, 526989, 526989, 526989, 526989, 526989, 526989, 526989, 526989, 526989, 107611, NULL, 269028, 269028, 269028, 269028, 269028, 269028, 1449220, 1449220, 1449220, 1449220, 1449220, 1449220, 1449220, 1449220, 1449220, 1449220, 1449220, 1449220, 1449220, NULL, 107611, 107611, 107611, 107611, 107611, 107611, 526989, 526989, 526989, 526989, 526989, 526989, 526989, 526989, 526989, 526989, 526989, 526989, 526989, 526989, 526989, 526989, 526989, 526989, 526989);
INSERT INTO `jobexp` VALUES (41, NULL, 125915, 125915, 125915, 125915, 125915, 125915, 610246, 610246, 610246, 610246, 610246, 610246, 610246, 610246, 610246, 610246, 610246, 610246, 610246, 125915, NULL, 314788, 314788, 314788, 314788, 314788, 314788, 1678177, 1678177, 1678177, 1678177, 1678177, 1678177, 1678177, 1678177, 1678177, 1678177, 1678177, 1678177, 1678177, NULL, 125915, 125915, 125915, 125915, 125915, 125915, 610246, 610246, 610246, 610246, 610246, 610246, 610246, 610246, 610246, 610246, 610246, 610246, 610246, 610246, 610246, 610246, 610246, 610246, 610246);
INSERT INTO `jobexp` VALUES (42, NULL, 153941, 153941, 153941, 153941, 153941, 153941, 644736, 644736, 644736, 644736, 644736, 644736, 644736, 644736, 644736, 644736, 644736, 644736, 644736, 153941, NULL, 384853, 384853, 384853, 384853, 384853, 384853, 1773024, 1773024, 1773024, 1773024, 1773024, 1773024, 1773024, 1773024, 1773024, 1773024, 1773024, 1773024, 1773024, NULL, 153941, 153941, 153941, 153941, 153941, 153941, 644736, 644736, 644736, 644736, 644736, 644736, 644736, 644736, 644736, 644736, 644736, 644736, 644736, 644736, 644736, 644736, 644736, 644736, 644736);
INSERT INTO `jobexp` VALUES (43, NULL, 191781, 191781, 191781, 191781, 191781, 191781, 793535, 793535, 793535, 793535, 793535, 793535, 793535, 793535, 793535, 793535, 793535, 793535, 793535, 191781, NULL, 479453, 479453, 479453, 479453, 479453, 479453, 2182221, 2182221, 2182221, 2182221, 2182221, 2182221, 2182221, 2182221, 2182221, 2182221, 2182221, 2182221, 2182221, NULL, 191781, 191781, 191781, 191781, 191781, 191781, 793535, 793535, 793535, 793535, 793535, 793535, 793535, 793535, 793535, 793535, 793535, 793535, 793535, 793535, 793535, 793535, 793535, 793535, 793535);
INSERT INTO `jobexp` VALUES (44, NULL, 204351, 204351, 204351, 204351, 204351, 204351, 921810, 921810, 921810, 921810, 921810, 921810, 921810, 921810, 921810, 921810, 921810, 921810, 921810, 204351, NULL, 510878, 510878, 510878, 510878, 510878, 510878, 2534978, 2534978, 2534978, 2534978, 2534978, 2534978, 2534978, 2534978, 2534978, 2534978, 2534978, 2534978, 2534978, NULL, 204351, 204351, 204351, 204351, 204351, 204351, 921810, 921810, 921810, 921810, 921810, 921810, 921810, 921810, 921810, 921810, 921810, 921810, 921810, 921810, 921810, 921810, 921810, 921810, 921810);
INSERT INTO `jobexp` VALUES (45, NULL, 248352, 248352, 248352, 248352, 248352, 248352, 1106758, 1106758, 1106758, 1106758, 1106758, 1106758, 1106758, 1106758, 1106758, 1106758, 1106758, 1106758, 1106758, 248352, NULL, 620880, 620880, 620880, 620880, 620880, 620880, 3043585, 3043585, 3043585, 3043585, 3043585, 3043585, 3043585, 3043585, 3043585, 3043585, 3043585, 3043585, 3043585, NULL, 248352, 248352, 248352, 248352, 248352, 248352, 1106758, 1106758, 1106758, 1106758, 1106758, 1106758, 1106758, 1106758, 1106758, 1106758, 1106758, 1106758, 1106758, 1106758, 1106758, 1106758, 1106758, 1106758, 1106758);
INSERT INTO `jobexp` VALUES (46, NULL, 286212, 286212, 286212, 286212, 286212, 286212, 1260955, 1260955, 1260955, 1260955, 1260955, 1260955, 1260955, 1260955, 1260955, 1260955, 1260955, 1260955, 1260955, 286212, NULL, 715530, 715530, 715530, 715530, 715530, 715530, 3782865, 3782865, 3782865, 3782865, 3782865, 3782865, 3782865, 3782865, 3782865, 3782865, 3782865, 3782865, 3782865, NULL, 286212, 286212, 286212, 286212, 286212, 286212, 1260955, 1260955, 1260955, 1260955, 1260955, 1260955, 1260955, 1260955, 1260955, 1260955, 1260955, 1260955, 1260955, 1260955, 1260955, 1260955, 1260955, 1260955, 1260955);
INSERT INTO `jobexp` VALUES (47, NULL, 386371, 386371, 386371, 386371, 386371, 386371, 1487304, 1487304, 1487304, 1487304, 1487304, 1487304, 1487304, 1487304, 1487304, 1487304, 1487304, 1487304, 1487304, 386371, NULL, 965928, 965928, 965928, 965928, 965928, 965928, 4461912, 4461912, 4461912, 4461912, 4461912, 4461912, 4461912, 4461912, 4461912, 4461912, 4461912, 4461912, 4461912, NULL, 386371, 386371, 386371, 386371, 386371, 386371, 1487304, 1487304, 1487304, 1487304, 1487304, 1487304, 1487304, 1487304, 1487304, 1487304, 1487304, 1487304, 1487304, 1487304, 1487304, 1487304, 1487304, 1487304, 1487304);
INSERT INTO `jobexp` VALUES (48, NULL, 409795, 409795, 409795, 409795, 409795, 409795, 1557657, 1557657, 1557657, 1557657, 1557657, 1557657, 1557657, 1557657, 1557657, 1557657, 1557657, 1557657, 1557657, 409795, NULL, 1024488, 1024488, 1024488, 1024488, 1024488, 1024488, 4672971, 4672971, 4672971, 4672971, 4672971, 4672971, 4672971, 4672971, 4672971, 4672971, 4672971, 4672971, 4672971, NULL, 409795, 409795, 409795, 409795, 409795, 409795, 1557657, 1557657, 1557657, 1557657, 1557657, 1557657, 1557657, 1557657, 1557657, 1557657, 1557657, 1557657, 1557657, 1557657, 1557657, 1557657, 1557657, 1557657, 1557657);
INSERT INTO `jobexp` VALUES (49, NULL, 482092, 482092, 482092, 482092, 482092, 482092, 1990632, 1990632, 1990632, 1990632, 1990632, 1990632, 1990632, 1990632, 1990632, 1990632, 1990632, 1990632, 1990632, 482092, NULL, 1205230, 1205230, 1205230, 1205230, 1205230, 1205230, 5971896, 5971896, 5971896, 5971896, 5971896, 5971896, 5971896, 5971896, 5971896, 5971896, 5971896, 5971896, 5971896, NULL, 482092, 482092, 482092, 482092, 482092, 482092, 1990632, 1990632, 1990632, 1990632, 1990632, 1990632, 1990632, 1990632, 1990632, 1990632, 1990632, 1990632, 1990632, 1990632, 1990632, 1990632, 1990632, 1990632, 1990632);
INSERT INTO `jobexp` VALUES (50, NULL, 509596, 509596, 509596, 509596, 509596, 509596, 2083386, 2083386, 2083386, 2083386, 2083386, 2083386, 2083386, 2083386, 2083386, 2083386, 2083386, 2083386, 2083386, 509596, NULL, 1273990, 1273990, 1273990, 1273990, 1273990, 1273990, 6250158, 6250158, 6250158, 6250158, 6250158, 6250158, 6250158, 6250158, 6250158, 6250158, 6250158, 6250158, 6250158, NULL, 509596, 509596, 509596, 509596, 509596, 509596, 2083386, 2083386, 2083386, 2083386, 2083386, 2083386, 2083386, 2083386, 2083386, 2083386, 2083386, 2083386, 2083386, 2083386, 2083386, 2083386, 2083386, 2083386, 2083386);
INSERT INTO `jobexp` VALUES (51, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 6875174, 6875174, 6875174, 6875174, 6875174, 6875174, 6875174, 6875174, 6875174, 6875174, 6875174, 6875174, 6875174, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
INSERT INTO `jobexp` VALUES (52, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 7562691, 7562691, 7562691, 7562691, 7562691, 7562691, 7562691, 7562691, 7562691, 7562691, 7562691, 7562691, 7562691, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
INSERT INTO `jobexp` VALUES (53, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 8318960, 8318960, 8318960, 8318960, 8318960, 8318960, 8318960, 8318960, 8318960, 8318960, 8318960, 8318960, 8318960, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
INSERT INTO `jobexp` VALUES (54, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 9150856, 9150856, 9150856, 9150856, 9150856, 9150856, 9150856, 9150856, 9150856, 9150856, 9150856, 9150856, 9150856, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
INSERT INTO `jobexp` VALUES (55, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 10065942, 10065942, 10065942, 10065942, 10065942, 10065942, 10065942, 10065942, 10065942, 10065942, 10065942, 10065942, 10065942, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
INSERT INTO `jobexp` VALUES (56, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 11877812, 11877812, 11877812, 11877812, 11877812, 11877812, 11877812, 11877812, 11877812, 11877812, 11877812, 11877812, 11877812, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
INSERT INTO `jobexp` VALUES (57, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 14015818, 14015818, 14015818, 14015818, 14015818, 14015818, 14015818, 14015818, 14015818, 14015818, 14015818, 14015818, 14015818, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
INSERT INTO `jobexp` VALUES (58, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 16538665, 16538665, 16538665, 16538665, 16538665, 16538665, 16538665, 16538665, 16538665, 16538665, 16538665, 16538665, 16538665, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
INSERT INTO `jobexp` VALUES (59, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 19515624, 19515624, 19515624, 19515624, 19515624, 19515624, 19515624, 19515624, 19515624, 19515624, 19515624, 19515624, 19515624, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
INSERT INTO `jobexp` VALUES (60, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 23028437, 23028437, 23028437, 23028437, 23028437, 23028437, 23028437, 23028437, 23028437, 23028437, 23028437, 23028437, 23028437, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
INSERT INTO `jobexp` VALUES (61, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 28094693, 28094693, 28094693, 28094693, 28094693, 28094693, 28094693, 28094693, 28094693, 28094693, 28094693, 28094693, 28094693, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
INSERT INTO `jobexp` VALUES (62, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 34275525, 34275525, 34275525, 34275525, 34275525, 34275525, 34275525, 34275525, 34275525, 34275525, 34275525, 34275525, 34275525, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
INSERT INTO `jobexp` VALUES (63, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 41816141, 41816141, 41816141, 41816141, 41816141, 41816141, 41816141, 41816141, 41816141, 41816141, 41816141, 41816141, 41816141, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
INSERT INTO `jobexp` VALUES (64, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 51015692, 51015692, 51015692, 51015692, 51015692, 51015692, 51015692, 51015692, 51015692, 51015692, 51015692, 51015692, 51015692, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
INSERT INTO `jobexp` VALUES (65, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 62239144, 62239144, 62239144, 62239144, 62239144, 62239144, 62239144, 62239144, 62239144, 62239144, 62239144, 62239144, 62239144, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
INSERT INTO `jobexp` VALUES (66, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 79666104, 79666104, 79666104, 79666104, 79666104, 79666104, 79666104, 79666104, 79666104, 79666104, 79666104, 79666104, 79666104, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
INSERT INTO `jobexp` VALUES (67, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 101972614, 101972614, 101972614, 101972614, 101972614, 101972614, 101972614, 101972614, 101972614, 101972614, 101972614, 101972614, 101972614, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
INSERT INTO `jobexp` VALUES (68, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 130524946, 130524946, 130524946, 130524946, 130524946, 130524946, 130524946, 130524946, 130524946, 130524946, 130524946, 130524946, 130524946, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
INSERT INTO `jobexp` VALUES (69, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 167071930, 167071930, 167071930, 167071930, 167071930, 167071930, 167071930, 167071930, 167071930, 167071930, 167071930, 167071930, 167071930, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
INSERT INTO `jobexp` VALUES (70, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 213852071, 213852071, 213852071, 213852071, 213852071, 213852071, 213852071, 213852071, 213852071, 213852071, 213852071, 213852071, 213852071, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
INSERT INTO `jobexp` VALUES (71, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
INSERT INTO `jobexp` VALUES (72, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
INSERT INTO `jobexp` VALUES (73, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
INSERT INTO `jobexp` VALUES (74, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
INSERT INTO `jobexp` VALUES (75, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
INSERT INTO `jobexp` VALUES (76, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
INSERT INTO `jobexp` VALUES (77, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
INSERT INTO `jobexp` VALUES (78, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
INSERT INTO `jobexp` VALUES (79, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
INSERT INTO `jobexp` VALUES (80, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
INSERT INTO `jobexp` VALUES (81, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
INSERT INTO `jobexp` VALUES (82, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
INSERT INTO `jobexp` VALUES (83, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
INSERT INTO `jobexp` VALUES (84, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
INSERT INTO `jobexp` VALUES (85, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
INSERT INTO `jobexp` VALUES (86, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
INSERT INTO `jobexp` VALUES (87, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
INSERT INTO `jobexp` VALUES (88, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
INSERT INTO `jobexp` VALUES (89, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
INSERT INTO `jobexp` VALUES (90, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
INSERT INTO `jobexp` VALUES (91, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
INSERT INTO `jobexp` VALUES (92, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
INSERT INTO `jobexp` VALUES (93, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
INSERT INTO `jobexp` VALUES (94, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
INSERT INTO `jobexp` VALUES (95, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
INSERT INTO `jobexp` VALUES (96, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
INSERT INTO `jobexp` VALUES (97, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
INSERT INTO `jobexp` VALUES (98, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
INSERT INTO `jobexp` VALUES (99, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);

