-- MySQL dump 10.13  Distrib 5.5.20, for osx10.7 (i386)
--
-- Host: localhost    Database: movie_organizer
-- ------------------------------------------------------
-- Server version	5.5.14

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
-- Table structure for table `actors`
--

DROP TABLE IF EXISTS `actors`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `actors` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `name` varchar(512) NOT NULL DEFAULT '',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=28 DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `actors`
--

LOCK TABLES `actors` WRITE;
/*!40000 ALTER TABLE `actors` DISABLE KEYS */;
INSERT INTO `actors` VALUES (1,'Noomi Rapace'),(2,'Michael Fassbender'),(10,'Charlize Theron'),(11,'Idris Elba'),(12,'Guy Pearce'),(13,'Logan Marshall-Green'),(14,'Benedict Wong'),(15,'Emun Elliott'),(16,'Rafe Spall'),(17,'Sean Harris'),(18,'Christopher Reeve'),(19,'Richard Pryor'),(20,'Jackie Cooper'),(21,'Annette O\'Toole'),(22,'Marc McClure'),(23,'Bruce Willis'),(24,'Mary-Louise Parker'),(25,'Morgan Freeman'),(26,'John Malkovich'),(27,'Helen Mirren');
/*!40000 ALTER TABLE `actors` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `cast`
--

DROP TABLE IF EXISTS `cast`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `cast` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `actor_id` int(11) unsigned NOT NULL,
  `movie_id` int(11) unsigned NOT NULL,
  `character` varchar(512) NOT NULL DEFAULT '',
  PRIMARY KEY (`id`),
  KEY `movie` (`movie_id`),
  KEY `actor` (`actor_id`)
) ENGINE=InnoDB AUTO_INCREMENT=22 DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `cast`
--

LOCK TABLES `cast` WRITE;
/*!40000 ALTER TABLE `cast` DISABLE KEYS */;
INSERT INTO `cast` VALUES (2,1,1,'Elizabeth Shaw'),(3,2,1,'David'),(4,10,1,'Meredith Vickers'),(5,11,1,'Janek'),(6,12,1,'Peter Weyland'),(7,13,1,'Charlie Holloway'),(8,14,1,'Ravel'),(9,15,1,'Chance'),(10,16,1,'Millburn'),(11,17,1,'Fifield'),(12,18,2,'Superman / Clark Kent'),(13,19,2,'Gus Gorman'),(14,20,2,'Perry White'),(15,21,2,'Lana Lang'),(16,22,2,'Jimmy Olsen'),(17,23,3,'Frank Moses'),(18,24,3,'Sarah Ross'),(19,25,3,'Joe Matheson'),(20,26,3,'Marvin Boggs'),(21,27,3,'Victoria');
/*!40000 ALTER TABLE `cast` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `movies`
--

DROP TABLE IF EXISTS `movies`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `movies` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `title` varchar(512) NOT NULL DEFAULT '',
  `year` year(4) NOT NULL,
  `tagline` varchar(512) DEFAULT NULL,
  `poster` varchar(64) DEFAULT NULL,
  `synopsis` text,
  `plot` text,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=4 DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `movies`
--

LOCK TABLES `movies` WRITE;
/*!40000 ALTER TABLE `movies` DISABLE KEYS */;
INSERT INTO `movies` VALUES (1,'Prometheus',2012,'tyjktyjf',NULL,'A team of scientists journey through the universe on the spaceship \"Prometheus\" on a voyage to investigate Alien life forms. The team of scientists becomes stranded on an Alien world, and as they struggle to survive it becomes clear that the horrors they experience are not just a threat to themselves, but to all of mankind.','As two superpowers fight each other on Earth for the remaining natural resources throughout the solar system, a giant spaceship, known as the Prometheus, is sent to the distant planet of Erix to terraform the world, but problems arise when the crew discover the uninhabitable planet\'s indigenous life of bio-mechanoid killers.'),(2,'Superman III',1983,'',NULL,'Wealthy businessman Ross Webster discovers the hidden talents of Gus Gorman, a mischievous computer genius. Ross decides to abuse his talents, in a way to help Webster with his plans for economic control. When the man of steel interferes, something must be done about Supes. When Gus\' synthetic Kryptonite fails to kill Superman, it turns him in an evil incarnation of his former self. The tar-laced Kryptonite pits man against himself, setting up the Clark vs. Superman battle.','In mortal enemies, the Man of Steel has no match. Even faced with a trio of sinister super-powered villains from his home planet, Superman saved the day. But can super-strength stand up to the diabolical circuitry of a criminally insane computer? Enter Gus Gorman, a genial half-wit who just happens to be a natural-born genius at computer programming. In his hands, a computer keyboard turns into a deadly weapon . . . and soon, Superman faces the microelectronic menace of his career. Clark Kent meets his old flame Lana Lang at a Smallville High School reunion and Superman turns into his own worst enemy after exposure to a chunk of red kryptonite '),(3,'Red',2010,NULL,NULL,'When his peaceful life is threatened by a high-tech assassin, former black-ops agent Frank Moses reassembles his old team in a last ditch effort to survive and uncover his assailants.','Frank (Bruce Willis) is retired, bored and lonely living off his government pension in a nondescript suburb in an equally nondescript house. The only joy in Frank\'s life are his calls to the government pension processing center when he gets to talk to his case worker Sarah (Mary-Louis Parker). Sarah is as bored and lonely as Frank and marks her conversations with the unknown Frank and her spy novels as the only things fun in her life. When something in Frank\'s past forces Frank back into his old line of work and puts an unwitting Sarah in the middle of the intrigue, Frank and Sarah begin a journey into Franks past and the people he used to work with. Like Frank they are all RED ... Retired Extremely Dangerous. ');
/*!40000 ALTER TABLE `movies` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2012-05-09 20:05:32
