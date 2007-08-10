-- phpMyAdmin SQL Dump
-- version 2.8.2.4
-- http://www.phpmyadmin.net
-- 
-- Host: localhost
-- Generation Time: Jan 17, 2007 at 09:00 AM
-- Server version: 5.0.24
-- PHP Version: 5.1.6
-- 
-- Database: `heliosgame`
-- 

-- --------------------------------------------------------

-- 
-- Table structure for table `cart_inventory`
-- 

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
) ENGINE=MyISAM DEFAULT CHARSET=latin1 AUTO_INCREMENT=1 ;

-- 
-- Dumping data for table `cart_inventory`
-- 


-- --------------------------------------------------------

-- 
-- Table structure for table `characters`
-- 

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
  `righthand` int(11) unsigned NOT NULL default '0',
  `lefthand` int(11) unsigned NOT NULL default '0',
  `armor` int(11) unsigned NOT NULL default '0',
  `garment` int(11) unsigned NOT NULL default '0',
  `shoes` int(11) unsigned NOT NULL default '0',
  `accessory1` int(11) unsigned NOT NULL default '0',
  `accessory2` int(11) unsigned NOT NULL default '0',
  `head_top` int(11) unsigned NOT NULL default '0',
  `head_mid` int(11) unsigned NOT NULL default '0',
  `head_bottom` int(11) unsigned NOT NULL default '0',
  `last_map` varchar(20) NOT NULL default 'new_1-1.gat',
  `last_x` int(4) NOT NULL default '53',
  `last_y` int(11) NOT NULL default '111',
  `save_map` varchar(20) NOT NULL default 'new_1-1.gat',
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
) ENGINE=MyISAM AUTO_INCREMENT=53 DEFAULT CHARSET=latin1 AUTO_INCREMENT=53;

-- 
-- Dumping data for table `characters`
-- 


-- --------------------------------------------------------
-- 
-- Table structure for table `character_vars`
-- 

CREATE TABLE `character_vars` (
  `char_id` int(11) NOT NULL default '0',
  `key` varchar(255) NOT NULL default '',
  `value` int(11) NOT NULL default '0',
  PRIMARY KEY  (`char_id`,`key`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

-- 
-- Dumping data for table `character_vars`
-- 


-- --------------------------------------------------------
-- 
-- Table structure for table `friend`
-- 

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


-- --------------------------------------------------------



-- 
-- Table structure for table `guild`
-- 

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


-- --------------------------------------------------------

-- 
-- Table structure for table `guild_alliance`
-- 

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


-- --------------------------------------------------------

-- 
-- Table structure for table `guild_castle`
-- 

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


-- --------------------------------------------------------

-- 
-- Table structure for table `guild_expulsion`
-- 

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


-- --------------------------------------------------------

-- 
-- Table structure for table `guild_member`
-- 

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


-- --------------------------------------------------------

-- 
-- Table structure for table `guild_position`
-- 

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


-- --------------------------------------------------------

-- 
-- Table structure for table `guild_skill`
-- 

CREATE TABLE `guild_skill` (
  `guild_id` int(11) NOT NULL default '0',
  `id` int(11) NOT NULL default '0',
  `lv` int(11) NOT NULL default '0',
  KEY `guild_id` (`guild_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

-- 
-- Dumping data for table `guild_skill`
-- 


-- --------------------------------------------------------

-- 
-- Table structure for table `guild_storage`
-- 

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
) ENGINE=MyISAM DEFAULT CHARSET=latin1 AUTO_INCREMENT=1 ;

-- 
-- Dumping data for table `guild_storage`
-- 


-- --------------------------------------------------------

-- 
-- Table structure for table `homunculus`
-- 

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
) ENGINE=MyISAM DEFAULT CHARSET=latin1 AUTO_INCREMENT=1 ;

-- 
-- Dumping data for table `homunculus`
-- 


-- --------------------------------------------------------

-- 
-- Table structure for table `homunculus_skill`
-- 

CREATE TABLE `homunculus_skill` (
  `homun_id` int(11) NOT NULL default '0',
  `id` int(11) NOT NULL default '0',
  `lv` int(11) NOT NULL default '0',
  KEY `homun_id` (`homun_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

-- 
-- Dumping data for table `homunculus_skill`
-- 


-- --------------------------------------------------------

-- 
-- Table structure for table `inventory`
-- 

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
) ENGINE=MyISAM DEFAULT CHARSET=latin1 AUTO_INCREMENT=1 ;

-- 
-- Dumping data for table `inventory`
-- 


-- --------------------------------------------------------

-- 
-- Table structure for table `memo`
-- 

CREATE TABLE `memo` (
  `memo_id` int(11) NOT NULL auto_increment,
  `char_id` int(11) NOT NULL default '0',
  `map` varchar(255) NOT NULL default '',
  `x` mediumint(9) NOT NULL default '0',
  `y` mediumint(9) NOT NULL default '0',
  PRIMARY KEY  (`memo_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 AUTO_INCREMENT=1 ;

-- 
-- Dumping data for table `memo`
-- 


-- --------------------------------------------------------

-- 
-- Table structure for table `party`
-- 

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


-- --------------------------------------------------------

-- 
-- Table structure for table `pet`
-- 

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
) ENGINE=MyISAM DEFAULT CHARSET=latin1 AUTO_INCREMENT=1 ;

-- 
-- Dumping data for table `pet`
-- 


-- --------------------------------------------------------

-- 
-- Table structure for table `ragsrvinfo`
-- 

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


-- --------------------------------------------------------

-- 
-- Table structure for table `skill`
-- 

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


-- --------------------------------------------------------

-- 
-- Table structure for table `sstatus`
-- 

CREATE TABLE `sstatus` (
  `index` tinyint(4) NOT NULL default '0',
  `name` varchar(255) NOT NULL default '',
  `user` int(11) NOT NULL default '0'
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

-- 
-- Dumping data for table `sstatus`
-- 


-- --------------------------------------------------------

-- 
-- Table structure for table `storage`
-- 

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
) ENGINE=MyISAM DEFAULT CHARSET=latin1 AUTO_INCREMENT=1 ;

-- 
-- Dumping data for table `storage`
-- 

