-- MySQL dump 10.10
--
-- Host: localhost    Database: helioscommon
-- ------------------------------------------------------
-- Server version	5.0.24a-community-nt

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Current Database: `helioscommon`
--

CREATE DATABASE IF NOT EXISTS`helioscommon` DEFAULT CHARACTER SET utf8;

USE `helioscommon`;

--
-- Table structure for table `accounts`
--

DROP TABLE IF EXISTS `accounts`;
CREATE TABLE `accounts` (
  `account_id` int(11) NOT NULL auto_increment,
  `userid` varchar(255) NOT NULL default '',
  `user_pass` varchar(32) NOT NULL default '',
  `lastlogin` datetime NOT NULL default '0000-00-00 00:00:00',
  `sex` char(1) NOT NULL default 'M',
  `logincount` mediumint(9) NOT NULL default '0',
  `email` varchar(60) NOT NULL default '-@-',
  `loginkey1` int(11) NOT NULL default '0',
  `loginkey2` int(11) NOT NULL default '0',
  `level` smallint(3) NOT NULL default '0',
  `error_message` int(11) NOT NULL default '0',
  `connect_until` datetime NOT NULL default '9999-12-31 23:59:59',
  `last_ip` varchar(100) NOT NULL default '',
  `memo` int(11) NOT NULL default '0',
  `ban_until` datetime NOT NULL default '0000-00-00 00:00:00',
  `state` int(11) NOT NULL default '0',
  PRIMARY KEY  (`account_id`),
  KEY `name` (`userid`)
) ENGINE=MyISAM AUTO_INCREMENT=100101 DEFAULT CHARSET=latin1;

--
-- Dumping data for table `accounts`
--


/*!40000 ALTER TABLE `accounts` DISABLE KEYS */;
LOCK TABLES `accounts` WRITE;
INSERT INTO `accounts` VALUES (100100,'test','test','2006-11-14 19:37:15','M',10,'-@-',1057479608,1647175830,1,0,'9999-12-31 23:59:59','192.168.10.101',0,'1900-01-01 00:00:00',0);
UNLOCK TABLES;
/*!40000 ALTER TABLE `accounts` ENABLE KEYS */;

--
-- Table structure for table `interlog`
--

DROP TABLE IF EXISTS `interlog`;
CREATE TABLE `interlog` (
  `time` datetime NOT NULL default '0000-00-00 00:00:00',
  `log` varchar(255) NOT NULL default ''
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `interlog`
--


/*!40000 ALTER TABLE `interlog` DISABLE KEYS */;
LOCK TABLES `interlog` WRITE;
UNLOCK TABLES;
/*!40000 ALTER TABLE `interlog` ENABLE KEYS */;

--
-- Table structure for table `ipbanlist`
--

DROP TABLE IF EXISTS `ipbanlist`;
CREATE TABLE `ipbanlist` (
  `list` varchar(255) NOT NULL default '',
  `btime` datetime NOT NULL default '0000-00-00 00:00:00',
  `rtime` datetime NOT NULL default '0000-00-00 00:00:00',
  `reason` varchar(255) NOT NULL default ''
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `ipbanlist`
--


/*!40000 ALTER TABLE `ipbanlist` DISABLE KEYS */;
LOCK TABLES `ipbanlist` WRITE;
UNLOCK TABLES;
/*!40000 ALTER TABLE `ipbanlist` ENABLE KEYS */;

--
-- Table structure for table `login_error`
--

DROP TABLE IF EXISTS `login_error`;
CREATE TABLE `login_error` (
  `err_id` int(11) NOT NULL default '0',
  `reason` varchar(100) NOT NULL default 'Unknown',
  PRIMARY KEY  (`err_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `login_error`
--


/*!40000 ALTER TABLE `login_error` DISABLE KEYS */;
LOCK TABLES `login_error` WRITE;
UNLOCK TABLES;
/*!40000 ALTER TABLE `login_error` ENABLE KEYS */;

--
-- Table structure for table `loginlog`
--

DROP TABLE IF EXISTS `loginlog`;
CREATE TABLE `loginlog` (
  `time` datetime NOT NULL default '0000-00-00 00:00:00',
  `ip` varchar(64) NOT NULL default '',
  `user` varchar(32) NOT NULL default '',
  `rcode` tinyint(4) NOT NULL default '0',
  `log` varchar(255) NOT NULL default ''
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `loginlog`
--


/*!40000 ALTER TABLE `loginlog` DISABLE KEYS */;
LOCK TABLES `loginlog` WRITE;
UNLOCK TABLES;
/*!40000 ALTER TABLE `loginlog` ENABLE KEYS */;


--
-- Current Database: `heliosgame`
--

CREATE DATABASE IF NOT EXISTS `heliosgame` DEFAULT CHARACTER SET utf8;

USE `heliosgame`;

--
-- Table structure for table `cart_inventory`
--

DROP TABLE IF EXISTS `cart_inventory`;
CREATE TABLE `cart_inventory` (
  `id` bigint(20) unsigned NOT NULL auto_increment,
  `char_id` int(11) NOT NULL default '0',
  `nameid` int(11) NOT NULL default '0',
  `amount` int(11) NOT NULL default '0',
  `equip` mediumint(8) unsigned NOT NULL default '0',
  `identify` smallint(6) NOT NULL default '0',
  `refine` tinyint(3) unsigned NOT NULL default '0',
  `attribute` tinyint(4) NOT NULL default '0',
  `card0` int(11) NOT NULL default '0',
  `card1` int(11) NOT NULL default '0',
  `card2` int(11) NOT NULL default '0',
  `card3` int(11) NOT NULL default '0',
  `broken` int(11) NOT NULL default '0',
  PRIMARY KEY  (`id`),
  KEY `char_id` (`char_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `cart_inventory`
--


/*!40000 ALTER TABLE `cart_inventory` DISABLE KEYS */;
LOCK TABLES `cart_inventory` WRITE;
UNLOCK TABLES;
/*!40000 ALTER TABLE `cart_inventory` ENABLE KEYS */;

--
-- Table structure for table `characters`
--

DROP TABLE IF EXISTS `characters`;
CREATE TABLE `characters` (
  `char_id` int(11) NOT NULL auto_increment,
  `account_id` int(11) NOT NULL default '0',
  `char_num` tinyint(4) NOT NULL default '0',
  `name` varchar(255) NOT NULL default '',
  `class` int(11) NOT NULL default '0',
  `base_level` bigint(20) unsigned NOT NULL default '1',
  `job_level` bigint(20) unsigned NOT NULL default '1',
  `base_exp` bigint(20) NOT NULL default '0',
  `job_exp` bigint(20) NOT NULL default '0',
  `zeny` int(11) NOT NULL default '500',
  `p_str` int(10) unsigned NOT NULL default '0',
  `p_agi` int(10) unsigned NOT NULL default '0',
  `p_vit` int(10) unsigned NOT NULL default '0',
  `p_int` int(10) unsigned NOT NULL default '0',
  `p_dex` int(10) unsigned NOT NULL default '0',
  `p_luk` int(10) unsigned NOT NULL default '0',
  `max_hp` int(11) NOT NULL default '0',
  `hp` int(11) NOT NULL default '0',
  `max_sp` int(11) NOT NULL default '0',
  `sp` int(11) NOT NULL default '0',
  `status_point` int(11) NOT NULL default '0',
  `skill_point` int(11) NOT NULL default '0',
  `options` int(11) NOT NULL default '0',
  `karma` int(11) NOT NULL default '0',
  `manner` int(11) NOT NULL default '0',
  `party_id` int(11) NOT NULL default '0',
  `guild_id` int(11) NOT NULL default '0',
  `pet_id` int(11) NOT NULL default '0',
  `hair` tinyint(4) NOT NULL default '0',
  `hair_color` int(11) NOT NULL default '0',
  `clothes_color` tinyint(4) NOT NULL default '0',
  `weapon` int(11) NOT NULL default '1',
  `shield` int(11) NOT NULL default '0',
  `head_top` int(11) NOT NULL default '0',
  `head_mid` int(11) NOT NULL default '0',
  `head_bottom` int(11) NOT NULL default '0',
  `last_map` varchar(20) NOT NULL default 'new_5-1.gat',
  `last_x` int(4) NOT NULL default '53',
  `last_y` int(11) NOT NULL default '111',
  `save_map` varchar(20) NOT NULL default 'new_5-1.gat',
  `save_x` int(11) NOT NULL default '53',
  `save_y` int(11) NOT NULL default '111',
  `partner_id` int(11) NOT NULL default '0',
  `parent_id` int(11) NOT NULL default '0',
  `parent_id2` int(11) NOT NULL default '0',
  `baby_id` int(11) NOT NULL default '0',
  `online` tinyint(4) NOT NULL default '0',
  `homun_id` int(11) NOT NULL default '0',
  PRIMARY KEY  (`char_id`),
  KEY `account_id` (`account_id`),
  KEY `party_id` (`party_id`),
  KEY `guild_id` (`guild_id`),
  KEY `name` (`name`)
) ENGINE=MyISAM AUTO_INCREMENT=3 DEFAULT CHARSET=latin1;

--
-- Dumping data for table `characters`
--


/*!40000 ALTER TABLE `characters` DISABLE KEYS */;
LOCK TABLES `characters` WRITE;
UNLOCK TABLES;
/*!40000 ALTER TABLE `characters` ENABLE KEYS */;

--
-- Table structure for table `friend`
--

DROP TABLE IF EXISTS `friend`;
CREATE TABLE `friend` (
  `char_id` int(11) NOT NULL default '0',
  `id1` int(11) NOT NULL default '0',
  `id2` int(11) NOT NULL default '0',
  `name` varchar(24) NOT NULL default '',
  KEY `char_id` (`char_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `friend`
--


/*!40000 ALTER TABLE `friend` DISABLE KEYS */;
LOCK TABLES `friend` WRITE;
UNLOCK TABLES;
/*!40000 ALTER TABLE `friend` ENABLE KEYS */;

--
-- Table structure for table `global_reg_value`
--

DROP TABLE IF EXISTS `global_reg_value`;
CREATE TABLE `global_reg_value` (
  `char_id` int(11) NOT NULL default '0',
  `str` varchar(255) NOT NULL default '',
  `value` varchar(255) NOT NULL default '0',
  `type` int(11) NOT NULL default '3',
  `account_id` int(11) NOT NULL default '0',
  PRIMARY KEY  (`char_id`,`str`,`account_id`),
  KEY `account_id` (`account_id`),
  KEY `char_id` (`char_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `global_reg_value`
--


/*!40000 ALTER TABLE `global_reg_value` DISABLE KEYS */;
LOCK TABLES `global_reg_value` WRITE;
UNLOCK TABLES;
/*!40000 ALTER TABLE `global_reg_value` ENABLE KEYS */;

--
-- Table structure for table `guild`
--

DROP TABLE IF EXISTS `guild`;
CREATE TABLE `guild` (
  `guild_id` int(11) NOT NULL default '10000',
  `name` varchar(24) NOT NULL default '',
  `master` varchar(24) NOT NULL default '',
  `guild_lv` smallint(6) NOT NULL default '0',
  `connect_member` smallint(6) NOT NULL default '0',
  `max_member` smallint(6) NOT NULL default '0',
  `average_lv` smallint(6) NOT NULL default '0',
  `exp` int(11) NOT NULL default '0',
  `next_exp` int(11) NOT NULL default '0',
  `skill_point` int(11) NOT NULL default '0',
  `castle_id` int(11) NOT NULL default '-1',
  `mes1` varchar(60) NOT NULL default '',
  `mes2` varchar(120) NOT NULL default '',
  `emblem_len` int(11) NOT NULL default '0',
  `emblem_id` int(11) NOT NULL default '0',
  `emblem_data` blob NOT NULL,
  PRIMARY KEY  (`guild_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `guild`
--


/*!40000 ALTER TABLE `guild` DISABLE KEYS */;
LOCK TABLES `guild` WRITE;
UNLOCK TABLES;
/*!40000 ALTER TABLE `guild` ENABLE KEYS */;

--
-- Table structure for table `guild_alliance`
--

DROP TABLE IF EXISTS `guild_alliance`;
CREATE TABLE `guild_alliance` (
  `guild_id` int(11) NOT NULL default '0',
  `opposition` int(11) NOT NULL default '0',
  `alliance_id` int(11) NOT NULL default '0',
  `name` varchar(24) NOT NULL default '',
  KEY `guild_id` (`guild_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `guild_alliance`
--


/*!40000 ALTER TABLE `guild_alliance` DISABLE KEYS */;
LOCK TABLES `guild_alliance` WRITE;
UNLOCK TABLES;
/*!40000 ALTER TABLE `guild_alliance` ENABLE KEYS */;

--
-- Table structure for table `guild_castle`
--

DROP TABLE IF EXISTS `guild_castle`;
CREATE TABLE `guild_castle` (
  `castle_id` int(11) NOT NULL default '0',
  `guild_id` int(11) NOT NULL default '0',
  `economy` int(11) NOT NULL default '0',
  `defense` int(11) NOT NULL default '0',
  `triggerE` int(11) NOT NULL default '0',
  `triggerD` int(11) NOT NULL default '0',
  `nextTime` int(11) NOT NULL default '0',
  `payTime` int(11) NOT NULL default '0',
  `createTime` int(11) NOT NULL default '0',
  `visibleC` int(11) NOT NULL default '0',
  `visibleG0` int(11) NOT NULL default '0',
  `visibleG1` int(11) NOT NULL default '0',
  `visibleG2` int(11) NOT NULL default '0',
  `visibleG3` int(11) NOT NULL default '0',
  `visibleG4` int(11) NOT NULL default '0',
  `visibleG5` int(11) NOT NULL default '0',
  `visibleG6` int(11) NOT NULL default '0',
  `visibleG7` int(11) NOT NULL default '0',
  `gHP0` int(11) NOT NULL default '0',
  `ghP1` int(11) NOT NULL default '0',
  `gHP2` int(11) NOT NULL default '0',
  `gHP3` int(11) NOT NULL default '0',
  `gHP4` int(11) NOT NULL default '0',
  `gHP5` int(11) NOT NULL default '0',
  `gHP6` int(11) NOT NULL default '0',
  `gHP7` int(11) NOT NULL default '0',
  PRIMARY KEY  (`castle_id`),
  KEY `guild_id` (`guild_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `guild_castle`
--


/*!40000 ALTER TABLE `guild_castle` DISABLE KEYS */;
LOCK TABLES `guild_castle` WRITE;
UNLOCK TABLES;
/*!40000 ALTER TABLE `guild_castle` ENABLE KEYS */;

--
-- Table structure for table `guild_expulsion`
--

DROP TABLE IF EXISTS `guild_expulsion`;
CREATE TABLE `guild_expulsion` (
  `guild_id` int(11) NOT NULL default '0',
  `name` varchar(24) NOT NULL default '',
  `mes` varchar(40) NOT NULL default '',
  `acc` varchar(40) NOT NULL default '',
  `account_id` int(11) NOT NULL default '0',
  `rsv1` int(11) NOT NULL default '0',
  `rsv2` int(11) NOT NULL default '0',
  `rsv3` int(11) NOT NULL default '0',
  KEY `guild_id` (`guild_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `guild_expulsion`
--


/*!40000 ALTER TABLE `guild_expulsion` DISABLE KEYS */;
LOCK TABLES `guild_expulsion` WRITE;
UNLOCK TABLES;
/*!40000 ALTER TABLE `guild_expulsion` ENABLE KEYS */;

--
-- Table structure for table `guild_member`
--

DROP TABLE IF EXISTS `guild_member`;
CREATE TABLE `guild_member` (
  `guild_id` int(11) NOT NULL default '0',
  `account_id` int(11) NOT NULL default '0',
  `char_id` int(11) NOT NULL default '0',
  `hair` smallint(6) NOT NULL default '0',
  `hair_color` smallint(6) NOT NULL default '0',
  `gender` smallint(6) NOT NULL default '0',
  `class` smallint(6) NOT NULL default '0',
  `lv` smallint(6) NOT NULL default '0',
  `exp` bigint(20) NOT NULL default '0',
  `exp_payper` int(11) NOT NULL default '0',
  `online` tinyint(4) NOT NULL default '0',
  `position` smallint(6) NOT NULL default '0',
  `rsv1` int(11) NOT NULL default '0',
  `rsv2` int(11) NOT NULL default '0',
  `name` varchar(24) NOT NULL default '',
  KEY `guild_id` (`guild_id`),
  KEY `account_id` (`account_id`),
  KEY `char_id` (`char_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `guild_member`
--


/*!40000 ALTER TABLE `guild_member` DISABLE KEYS */;
LOCK TABLES `guild_member` WRITE;
UNLOCK TABLES;
/*!40000 ALTER TABLE `guild_member` ENABLE KEYS */;

--
-- Table structure for table `guild_position`
--

DROP TABLE IF EXISTS `guild_position`;
CREATE TABLE `guild_position` (
  `guild_id` int(11) NOT NULL default '0',
  `position` smallint(6) NOT NULL default '0',
  `name` varchar(24) NOT NULL default '',
  `mode` int(11) NOT NULL default '0',
  `exp_mode` int(11) NOT NULL default '0',
  KEY `guild_id` (`guild_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `guild_position`
--


/*!40000 ALTER TABLE `guild_position` DISABLE KEYS */;
LOCK TABLES `guild_position` WRITE;
UNLOCK TABLES;
/*!40000 ALTER TABLE `guild_position` ENABLE KEYS */;

--
-- Table structure for table `guild_skill`
--

DROP TABLE IF EXISTS `guild_skill`;
CREATE TABLE `guild_skill` (
  `guild_id` int(11) NOT NULL default '0',
  `id` int(11) NOT NULL default '0',
  `lv` int(11) NOT NULL default '0',
  KEY `guild_id` (`guild_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `guild_skill`
--


/*!40000 ALTER TABLE `guild_skill` DISABLE KEYS */;
LOCK TABLES `guild_skill` WRITE;
UNLOCK TABLES;
/*!40000 ALTER TABLE `guild_skill` ENABLE KEYS */;

--
-- Table structure for table `guild_storage`
--

DROP TABLE IF EXISTS `guild_storage`;
CREATE TABLE `guild_storage` (
  `id` bigint(20) unsigned NOT NULL auto_increment,
  `guild_id` int(11) NOT NULL default '0',
  `nameid` int(11) NOT NULL default '0',
  `amount` int(11) NOT NULL default '0',
  `equip` mediumint(8) unsigned NOT NULL default '0',
  `identify` smallint(6) NOT NULL default '0',
  `refine` tinyint(3) unsigned NOT NULL default '0',
  `attribute` tinyint(4) NOT NULL default '0',
  `card0` int(11) NOT NULL default '0',
  `card1` int(11) NOT NULL default '0',
  `card2` int(11) NOT NULL default '0',
  `card3` int(11) NOT NULL default '0',
  `broken` int(11) NOT NULL default '0',
  PRIMARY KEY  (`id`),
  KEY `guild_id` (`guild_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `guild_storage`
--


/*!40000 ALTER TABLE `guild_storage` DISABLE KEYS */;
LOCK TABLES `guild_storage` WRITE;
UNLOCK TABLES;
/*!40000 ALTER TABLE `guild_storage` ENABLE KEYS */;

--
-- Table structure for table `homunculus`
--

DROP TABLE IF EXISTS `homunculus`;
CREATE TABLE `homunculus` (
  `homun_id` int(11) NOT NULL auto_increment,
  `class` mediumint(9) NOT NULL default '0',
  `name` varchar(24) NOT NULL default '',
  `account_id` int(11) NOT NULL default '0',
  `char_id` int(11) NOT NULL default '0',
  `base_level` tinyint(4) NOT NULL default '0',
  `base_exp` bigint(20) NOT NULL default '0',
  `max_hp` int(11) NOT NULL default '0',
  `hp` int(11) NOT NULL default '0',
  `max_sp` int(11) NOT NULL default '0',
  `sp` int(11) NOT NULL default '0',
  `str` int(11) unsigned NOT NULL default '0',
  `agi` int(11) unsigned NOT NULL default '0',
  `vit` int(11) unsigned NOT NULL default '0',
  `int` int(11) unsigned NOT NULL default '0',
  `dex` int(11) unsigned NOT NULL default '0',
  `luk` int(11) unsigned NOT NULL default '0',
  `status_point` int(11) NOT NULL default '0',
  `skill_point` int(11) NOT NULL default '0',
  `equip` mediumint(8) unsigned NOT NULL default '0',
  `intimate` mediumint(9) NOT NULL default '0',
  `hungry` mediumint(9) NOT NULL default '0',
  `rename_flag` tinyint(4) NOT NULL default '0',
  `incubate` int(11) NOT NULL default '0',
  PRIMARY KEY  (`homun_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `homunculus`
--


/*!40000 ALTER TABLE `homunculus` DISABLE KEYS */;
LOCK TABLES `homunculus` WRITE;
UNLOCK TABLES;
/*!40000 ALTER TABLE `homunculus` ENABLE KEYS */;

--
-- Table structure for table `homunculus_skill`
--

DROP TABLE IF EXISTS `homunculus_skill`;
CREATE TABLE `homunculus_skill` (
  `homun_id` int(11) NOT NULL default '0',
  `id` int(11) NOT NULL default '0',
  `lv` int(11) NOT NULL default '0',
  KEY `homun_id` (`homun_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `homunculus_skill`
--


/*!40000 ALTER TABLE `homunculus_skill` DISABLE KEYS */;
LOCK TABLES `homunculus_skill` WRITE;
UNLOCK TABLES;
/*!40000 ALTER TABLE `homunculus_skill` ENABLE KEYS */;

--
-- Table structure for table `inventory`
--

DROP TABLE IF EXISTS `inventory`;
CREATE TABLE `inventory` (
  `id` bigint(20) unsigned NOT NULL auto_increment,
  `char_id` int(11) NOT NULL default '0',
  `nameid` int(11) NOT NULL default '0',
  `amount` int(11) NOT NULL default '0',
  `equip` mediumint(8) unsigned NOT NULL default '0',
  `identify` smallint(6) NOT NULL default '0',
  `refine` tinyint(3) unsigned NOT NULL default '0',
  `attribute` tinyint(4) NOT NULL default '0',
  `card0` int(11) NOT NULL default '0',
  `card1` int(11) NOT NULL default '0',
  `card2` int(11) NOT NULL default '0',
  `card3` int(11) NOT NULL default '0',
  `broken` int(11) NOT NULL default '0',
  PRIMARY KEY  (`id`),
  KEY `char_id` (`char_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `inventory`
--


/*!40000 ALTER TABLE `inventory` DISABLE KEYS */;
LOCK TABLES `inventory` WRITE;
UNLOCK TABLES;
/*!40000 ALTER TABLE `inventory` ENABLE KEYS */;

--
-- Table structure for table `memo`
--

DROP TABLE IF EXISTS `memo`;
CREATE TABLE `memo` (
  `memo_id` int(11) NOT NULL auto_increment,
  `char_id` int(11) NOT NULL default '0',
  `map` varchar(255) NOT NULL default '',
  `x` mediumint(9) NOT NULL default '0',
  `y` mediumint(9) NOT NULL default '0',
  PRIMARY KEY  (`memo_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `memo`
--


/*!40000 ALTER TABLE `memo` DISABLE KEYS */;
LOCK TABLES `memo` WRITE;
UNLOCK TABLES;
/*!40000 ALTER TABLE `memo` ENABLE KEYS */;

--
-- Table structure for table `party`
--

DROP TABLE IF EXISTS `party`;
CREATE TABLE `party` (
  `party_id` int(11) NOT NULL default '100',
  `name` char(100) NOT NULL default '',
  `exp` int(11) NOT NULL default '0',
  `item` int(11) NOT NULL default '0',
  `leader_id` int(11) NOT NULL default '0',
  PRIMARY KEY  (`party_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `party`
--


/*!40000 ALTER TABLE `party` DISABLE KEYS */;
LOCK TABLES `party` WRITE;
UNLOCK TABLES;
/*!40000 ALTER TABLE `party` ENABLE KEYS */;

--
-- Table structure for table `pet`
--

DROP TABLE IF EXISTS `pet`;
CREATE TABLE `pet` (
  `pet_id` int(11) NOT NULL auto_increment,
  `class` mediumint(9) NOT NULL default '0',
  `name` varchar(24) NOT NULL default '',
  `account_id` int(11) NOT NULL default '0',
  `char_id` int(11) NOT NULL default '0',
  `level` tinyint(4) NOT NULL default '0',
  `egg_id` int(11) NOT NULL default '0',
  `equip` mediumint(8) unsigned NOT NULL default '0',
  `intimate` mediumint(9) NOT NULL default '0',
  `hungry` mediumint(9) NOT NULL default '0',
  `rename_flag` tinyint(4) NOT NULL default '0',
  `incubate` int(11) NOT NULL default '0',
  PRIMARY KEY  (`pet_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `pet`
--


/*!40000 ALTER TABLE `pet` DISABLE KEYS */;
LOCK TABLES `pet` WRITE;
UNLOCK TABLES;
/*!40000 ALTER TABLE `pet` ENABLE KEYS */;

--
-- Table structure for table `ragsrvinfo`
--

DROP TABLE IF EXISTS `ragsrvinfo`;
CREATE TABLE `ragsrvinfo` (
  `index` int(11) NOT NULL default '0',
  `name` varchar(255) NOT NULL default '',
  `exp` int(11) NOT NULL default '0',
  `jexp` int(11) NOT NULL default '0',
  `drop` int(11) NOT NULL default '0',
  `motd` varchar(255) NOT NULL default ''
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `ragsrvinfo`
--


/*!40000 ALTER TABLE `ragsrvinfo` DISABLE KEYS */;
LOCK TABLES `ragsrvinfo` WRITE;
UNLOCK TABLES;
/*!40000 ALTER TABLE `ragsrvinfo` ENABLE KEYS */;

--
-- Table structure for table `skill`
--

DROP TABLE IF EXISTS `skill`;
CREATE TABLE `skill` (
  `char_id` int(11) NOT NULL default '0',
  `id` int(11) NOT NULL default '0',
  `lv` tinyint(4) NOT NULL default '0',
  PRIMARY KEY  (`char_id`,`id`),
  KEY `char_id` (`char_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `skill`
--


/*!40000 ALTER TABLE `skill` DISABLE KEYS */;
LOCK TABLES `skill` WRITE;
UNLOCK TABLES;
/*!40000 ALTER TABLE `skill` ENABLE KEYS */;

--
-- Table structure for table `sstatus`
--

DROP TABLE IF EXISTS `sstatus`;
CREATE TABLE `sstatus` (
  `index` tinyint(4) NOT NULL default '0',
  `name` varchar(255) NOT NULL default '',
  `user` int(11) NOT NULL default '0'
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `sstatus`
--


/*!40000 ALTER TABLE `sstatus` DISABLE KEYS */;
LOCK TABLES `sstatus` WRITE;
UNLOCK TABLES;
/*!40000 ALTER TABLE `sstatus` ENABLE KEYS */;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

--
-- Table structure for table `storage`
--

DROP TABLE IF EXISTS `storage`;
CREATE TABLE `storage` (
  `id` bigint(20) unsigned NOT NULL auto_increment,
  `account_id` int(11) NOT NULL default '0',
  `nameid` int(11) NOT NULL default '0',
  `amount` int(11) NOT NULL default '0',
  `equip` mediumint(8) unsigned NOT NULL default '0',
  `identify` smallint(6) NOT NULL default '0',
  `refine` tinyint(3) unsigned NOT NULL default '0',
  `attribute` tinyint(4) NOT NULL default '0',
  `card0` int(11) NOT NULL default '0',
  `card1` int(11) NOT NULL default '0',
  `card2` int(11) NOT NULL default '0',
  `card3` int(11) NOT NULL default '0',
  `broken` int(11) NOT NULL default '0',
  PRIMARY KEY  (`id`),
  KEY `account_id` (`account_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `storage`
--


/*!40000 ALTER TABLE `storage` DISABLE KEYS */;
LOCK TABLES `storage` WRITE;
UNLOCK TABLES;
/*!40000 ALTER TABLE `storage` ENABLE KEYS */;


/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

