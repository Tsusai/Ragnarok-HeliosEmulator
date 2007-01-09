-- phpMyAdmin SQL Dump
-- version 2.9.0.3
-- http://www.phpmyadmin.net
-- 
-- Host: localhost
-- Generation Time: Jan 09, 2007 at 05:33 PM
-- Server version: 5.0.27
-- PHP Version: 5.2.0
-- 
-- Database: `helioscommon`
-- 

-- --------------------------------------------------------

-- 
-- Table structure for table `accounts`
-- 

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
) ENGINE=MyISAM  DEFAULT CHARSET=latin1 AUTO_INCREMENT=100102 ;

-- 
-- Dumping data for table `accounts`
-- 

INSERT INTO `accounts` (`account_id`, `userid`, `user_pass`, `lastlogin`, `sex`, `logincount`, `email`, `loginkey1`, `loginkey2`, `level`, `error_message`, `connect_until`, `last_ip`, `memo`, `ban_until`, `state`) VALUES 
(100100, 'test', 'test', '2007-01-07 15:43:03', 'F', 147, '-@-', 1214329757, 225113618, 1, 0, '9999-12-31 23:59:59', '127.0.0.1', 0, '1900-01-01 00:00:00', 0);
