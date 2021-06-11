CREATE DATABASE IST659_PROJECT

/*TABLE 1
DROP AND CREATE PHYSICIANS TABLE*/

IF EXISTS (
	SELECT *
	FROM INFORMATION_SCHEMA.TABLES
	WHERE TABLE_NAME = 'PHYSICIANS'
)
BEGIN
	DROP TABLE PHYSICIANS
END

CREATE TABLE PHYSICIANS (
	PhysicianID int identity,
	FirstName varchar(50) not null,
	MiddleInitial char(1),
	LastName varchar(50) not null,
	CONSTRAINT PK_PHYSICIANS PRIMARY KEY (PhysicianID)
)



/*TABLE 2
DROP AND CREATE FACILITY TABLE*/

IF EXISTS (
	SELECT *
	FROM INFORMATION_SCHEMA.TABLES
	WHERE TABLE_NAME = 'FACILITY'
)
BEGIN
	DROP TABLE FACILITY
END

CREATE TABLE FACILITY (
	FacilityID int identity,
	FacilityName varchar(100) not null,
	CONSTRAINT PK_FACILITY PRIMARY KEY (FacilityID)
)

/*TABLE 3
DROP AND CREATE PHYSICIAN_FACILITY_LIST TABLE*/

IF EXISTS (
	SELECT *
	FROM INFORMATION_SCHEMA.TABLES
	WHERE TABLE_NAME = 'PHYSICIAN_FACILITY_LIST'
)
BEGIN
	DROP TABLE PHYSICIAN_FACILITY_LIST
END

CREATE TABLE PHYSICIAN_FACILITY_LIST (
	PhysicianFacilityListID int identity,
	PhysicianID int not null,
	FacilityID int not null,
	CONSTRAINT PK_PHYSICIAN_FACILITY_LIST PRIMARY KEY (PhysicianFacilityListID),
	CONSTRAINT FK1_PHYSICIAN_FACILITY_LIST FOREIGN KEY (PhysicianID) REFERENCES PHYSICIANS (PhysicianID),
	CONSTRAINT FK2_PHYSICIAN_FACILITY_LIST FOREIGN KEY (FacilityID) REFERENCES FACILITY (FacilityID),
	CONSTRAINT U1_PHYSICIAN_FACILITY_LIST UNIQUE (PhysicianID, FacilityID)
)

/*TABLE 4
DROP AND CREATE ADDRESSINFO TABLE*/

IF EXISTS (
	SELECT *
	FROM INFORMATION_SCHEMA.TABLES
	WHERE TABLE_NAME = 'ADDRESS_INFO'
)
BEGIN
	DROP TABLE ADDRESS_INFO
END

CREATE TABLE ADDRESS_INFO (
	AddressID int identity,
	Address1 varchar(100) not null,
	Address2 varchar(50) not null,
	City varchar(70) not null,
	ST char(2) not null,
	ZIP char(10) not null,
	CONSTRAINT PK_ADDRESS_INFO PRIMARY KEY (AddressID)
)

/*TABLE 5 
DROP AND CREATE PHYSICIAN_ADDRESS_LIST TABLE*/

IF EXISTS (
	SELECT *
	FROM INFORMATION_SCHEMA.TABLES
	WHERE TABLE_NAME = 'PHYSICIAN_ADDRESS_LIST'
)
BEGIN
	DROP TABLE PHYSICIAN_ADDRESS_LIST
END

CREATE TABLE PHYSICIAN_ADDRESS_LIST (
	PhysicianAddressListID int identity,
	PhysicianID int not null,
	AddressID int not null,
	CONSTRAINT PK_PHYSICIAN_ADDRESS_LIST PRIMARY KEY (PhysicianAddressListID),
	CONSTRAINT FK1_PHYSICIAN_ADDRESS_LIST FOREIGN KEY (PhysicianID) REFERENCES PHYSICIANS (PhysicianID),
	CONSTRAINT FK2_PHYSICIAN_ADDRESS_LIST FOREIGN KEY (AddressID) REFERENCES ADDRESS_INFO (AddressID),
	CONSTRAINT U1_PHYSICIAN_ADDRESS_LIST UNIQUE (PhysicianID, AddressID)
)

/*TABLE 6 
DROP AND CREATE FACILITY_ADDRESS_LIST TABLE*/

IF EXISTS (
	SELECT *
	FROM INFORMATION_SCHEMA.TABLES
	WHERE TABLE_NAME = 'FACILITY_ADDRESS_LIST'
)
BEGIN
	DROP TABLE FACILITY_ADDRESS_LIST
END

CREATE TABLE FACILITY_ADDRESS_LIST (
	FacilityAddressListID int identity,
	FacilityID int not null,
	AddressID int not null,
	CONSTRAINT PK_FACILITY_ADDRESS_LIST PRIMARY KEY (FacilityAddressListID),
	CONSTRAINT FK1_FACILITY_ADDRESS_LIST FOREIGN KEY (FacilityID) REFERENCES FACILITY(FacilityID),
	CONSTRAINT FK2_FACILITY_ADDRESS_LIST FOREIGN KEY (AddressID) REFERENCES ADDRESS_INFO(AddressID),
	CONSTRAINT U1_FACILITY_ADDRESS_LIST UNIQUE (FacilityID, AddressID)
)


/*TABLE 7 
DROP AND CREATE PBG TABLE*/

IF EXISTS (
	SELECT *
	FROM INFORMATION_SCHEMA.TABLES
	WHERE TABLE_NAME = 'PBG'
)
BEGIN
	DROP TABLE PBG
END

CREATE TABLE PBG (
	PBGID int identity PRIMARY KEY,
	PBGName varchar(70) not null UNIQUE,
)

/*TABLE 8 
DROP AND CREATE PHYSICIAN_PBG_LIST TABLE*/

IF EXISTS (
	SELECT *
	FROM INFORMATION_SCHEMA.TABLES
	WHERE TABLE_NAME = 'PHYSICIAN_PBG_LIST'
)
BEGIN
	DROP TABLE PHYSICIAN_PBG_LIST
END

CREATE TABLE PHYSICIAN_PBG_LIST (
	PhysicianPBGListID int identity PRIMARY KEY,
	PhysicianID int not null FOREIGN KEY REFERENCES PHYSICIANS(PhysicianID),
	PBGID int not null FOREIGN KEY REFERENCES PBG(PBGID),
)



/*TABLE 9
DROP AND CREATE IDN TABLE*/

IF EXISTS (
	SELECT *
	FROM INFORMATION_SCHEMA.TABLES
	WHERE TABLE_NAME = 'IDN'
)
BEGIN
	DROP TABLE IDN
END

CREATE TABLE IDN (
	IDNID int identity PRIMARY KEY,
	IDNName varchar(70) not null UNIQUE
)

/*TABLE 10
DROP AND CREATE PHYSICIAN_IDN_LIST TABLE*/

IF EXISTS (
	SELECT *
	FROM INFORMATION_SCHEMA.TABLES
	WHERE TABLE_NAME = 'PHYSICIAN_IDN_LIST'
)
BEGIN
	DROP TABLE PHYSICIAN_IDN_LIST
END

CREATE TABLE PHYSICIAN_IDN_LIST (
	PhysicianIDNListID int identity PRIMARY KEY,
	PhysicianID int not null FOREIGN KEY REFERENCES PHYSICIANS (PhysicianID),
	IDNID int not null FOREIGN KEY REFERENCES IDN(IDNID)
)


/*TABLE 11
DROP AND CREATE SPECIALTY*/

IF EXISTS (
	SELECT *
	FROM INFORMATION_SCHEMA.TABLES
	WHERE TABLE_NAME = 'SPECIALTY'
)
BEGIN
	DROP TABLE SPECIALTY
END

CREATE TABLE SPECIALTY (
	SpecialtyID int identity PRIMARY KEY,
	SpecialtyName varchar(70) not null UNIQUE
)

/*TABLE 12
DROP AND CREATE PHYSICIAN_SPECIALTY TABLE*/

IF EXISTS (
	SELECT *
	FROM INFORMATION_SCHEMA.TABLES
	WHERE TABLE_NAME = 'PHYSICIAN_SPECIALTY'
)
BEGIN
	DROP TABLE PHYSICIAN_SPECIALTY
END

CREATE TABLE PHYSICIAN_SPECIALTY (
	PhysicianSpecialtyID int identity PRIMARY KEY,
	PhysicianID int not null FOREIGN KEY REFERENCES PHYSICIANS(PhysicianID),
	SpecialtyID int not null FOREIGN KEY REFERENCES SPECIALTY(SpecialtyID)
	CONSTRAINT U1_PHYSICIAN_COT UNIQUE (PhysicianID, SpecialtyID)
)

/*TABLE 13
DROP AND CREATE DATES TABLE*/

IF EXISTS (
	SELECT *
	FROM INFORMATION_SCHEMA.TABLES
	WHERE TABLE_NAME = 'DATES'
)
BEGIN
	DROP TABLE DATES
END

CREATE TABLE DATES (
	DateID int identity PRIMARY KEY,
	Months varchar(9) not null, 
	Years smallint not null,
	CONSTRAINT U1_DATES UNIQUE (Months, Years)
)

/*TABLE 14
DROP AND CREATE PHYSICIAN_DATES TABLE*/

IF EXISTS (
	SELECT *
	FROM INFORMATION_SCHEMA.TABLES
	WHERE TABLE_NAME = 'PHYSICIAN_DATES'
)
BEGIN
	DROP TABLE PHYSICIAN_DATES
END

CREATE TABLE PHYSICIAN_DATES (
	PhysicianDatesID int identity PRIMARY KEY,
	PhysicianId int not null FOREIGN KEY REFERENCES PHYSICIANS(PhysicianID),
	DateID int not null FOREIGN KEY REFERENCES DATES(DateID)
)

/*TABLE 15
DROP AND CREATE FACILITY_DATES TABLE*/

IF EXISTS (
	SELECT *
	FROM INFORMATION_SCHEMA.TABLES
	WHERE TABLE_NAME = 'FACILITY_DATES'
)
BEGIN
	DROP TABLE FACILITY_DATES
END

CREATE TABLE FACILITY_DATES (
	FacilityDatesID int identity PRIMARY KEY,
	FacilityID int not null FOREIGN KEY REFERENCES FACILITY(FacilityID),
	DateID int not null FOREIGN KEY REFERENCES DATES(DateID)
)

/*TABLE 16
DROP AND CREATE PBG_DATES TABLE*/

IF EXISTS (
	SELECT *
	FROM INFORMATION_SCHEMA.TABLES
	WHERE TABLE_NAME = 'PBG_DATES'
)
BEGIN
	DROP TABLE PBG_DATES
END

CREATE TABLE PBG_DATES (
	PBGDatesID int identity PRIMARY KEY,
	PBGID int not null FOREIGN KEY REFERENCES PBG(PBGID),
	DateID int not null FOREIGN KEY REFERENCES DATES(DateID)
)

/*CREATE VIEW - 1
Identify full list to indicate where physicians are practicing. Physicians can practice in multiple locations.
Due to this being test date, actual locations are not to be interpreted as realistic*/
CREATE VIEW PHYSICIAN_LOCATIONS AS 
	SELECT P.FirstName, P.LastName, F.FacilityName, A.Address1, A.Address2, A.City, A.ST, A.ZIP
	FROM PHYSICIANS AS P
	JOIN PHYSICIAN_FACILITY_LIST AS PF ON P.PhysicianID = PF.PhysicianID
	JOIN FACILITY AS F ON PF.FacilityID = F.FacilityID
	JOIN FACILITY_ADDRESS_LIST	AS FA ON F.FacilityID = FA.FacilityID
	JOIN ADDRESS_INFO AS A ON FA.AddressID = A.AddressID
GO

/*CREATE VIEW - 2
Identify list of PBGs that physicians have been associated to*/
CREATE VIEW PHYSICIAN_PBG_AFFILIATION AS
	SELECT P.FirstName, P.LastName, P.Suffix, PBG.PBGName
	FROM  PHYSICIANS AS P
	JOIN PHYSICIAN_PBG_LIST AS PBL ON P.PhysicianID = PBL.PBGID
	JOIN PBG ON PBL.PBGID = PBG.PBGID
GO


/*CREATE VIEW - 3
Provides listing of when Physicians contracts were added and renewed*/
CREATE VIEW PHYSICIAN_ADDED AS
	SELECT P.FirstName, P.LastName, P.Suffix, D.Months, D.Years
	FROM PHYSICIANS AS P
	JOIN PHYSICIAN_DATES AS PD ON P.PhysicianID = PD.PhysicianId
	JOIN DATES AS D ON PD.DateID = D.DateID
	ORDER BY P.LastName OFFSET 0 ROWS
GO

/*CREATE FUNCTION - 1
Allows user to quickly identify the Physician ID, which can be useful when populating other tables
or for a WHERE clause*/
CREATE FUNCTION dbo.PhysicianIDLookup(@FirstName varchar(70), @LastName varchar(70), @Suffix varchar(5))
RETURNS int AS
BEGIN
	DECLARE @returnvalue int
	SELECT @returnvalue = PhysicianID FROM PHYSICIANS
	WHERE FirstName = @FirstName AND LastName = @LastName AND Suffix = @Suffix
	RETURN @returnvalue
END
GO

/*CREATE STORED PROCEDURE - 1
Instead of using Insert Into, create a procedure to end the information needed*/
CREATE PROCEDURE AddPhysician (@FirstName varchar(50), @MiddleInitial char(1), @LastName varchar(50), @Suffix varchar(4))
AS
BEGIN
	INSERT INTO PHYSICIANS
		(FirstName, MiddleInitial, LastName, Suffix)
	VALUES
		(@FirstName, @MiddleInitial, @LastName, @Suffix)
END
GO

/*INSERT INTO - 1*/
INSERT INTO PHYSICIANS 
	(FirstName, MiddleInitial, LastName, Suffix)
VALUES
	('Chris', 'M', 'Fandl', ''),
	('Mike', 'C', 'Trescavage', ''),
	('Lindsay', 'E', 'Holt', ''),
	('Joshua', 'W', 'Laird', ''),
	('Jennifer', '', 'Hufton', ''),
	('Maynard', 'J', 'Keenan', ''),
	('Jimmy', '', 'Page', ''),
	('Robert', '', 'Plant', ''),
	('Reggie', 'H', 'White', ''),
	('Jerome', 'W', 'Brown', 'III')
GO

/*INSERT INTO - 2*/
INSERT INTO ADDRESS_INFO
	(Address1, Address2, City, ST, ZIP)
VALUES
	('328 Elizabeth Drive', '', 'Kennett Square', 'PA', '19348'),
	('808 Outter Space Blvd', '', 'Camden', 'NJ', '01342'),
	('8 Plum Street', 'STE 1', 'Marcus Hook', 'GA', '68763'),
	('28 Oragne Drive', '', 'Jackson', 'CO', '89173'),
	('78 Brookline Road', '', 'Lateralus', 'CA', '90232'),
	('156 Whispers Way', '', 'Deptford', 'MO', '56943'),
	('1029 Walnut Lane', 'STE 2', 'Vicarious', 'MD', '27378'),
	('67 Arch Street', '', 'Maryton', 'FL', '43143'),
	('9256 Coastline Drive', '', 'Hilton Head', 'SC', '45221')
GO

/*INSERT INTO - 3*/
INSERT INTO PHYSICIAN_ADDRESS_LIST
	(PhysicianID, AddressID)
VALUES
	('1', '3'),
	('2', '4'),
	('2', '5'),
	('2', '8'),
	('3', '1'),
	('4', '3'),
	('4', '7'),
	('4', '6'),
	('5', '8'),
	('5', '2'),
	('6', '2'),
	('6', '5'),
	('7', '4'),
	('7', '7'),
	('7', '8'),
	('8', '2'),
	('9', '5'),
	('9', '6'),
	('9', '4'),
	('10', '3'),
	('10', '4'),
	('11', '5'),
	('11', '3'),
	('11', '4'),
	('11', '2'),
	('12', '7'),
	('12', '2'),
	('13', '6'),
	('13', '8'),
	('14', '4')
GO

/*INSERT INTO - 4*/
INSERT INTO FACILITY
	(FacilityName)
VALUES
	('Saint Christopher''s of Camden'),
	('First Response Care'),
	('Children''s Health of Florida'),
	('Family Awareness Practice'),
	('Hopewell Family Practice'),
	('Penn Medicine Peds'),
	('Rothman Institute'),
	('Careville Kids'),
	('The Pediatric Center')
GO

/*INSERT INTO - 5*/
INSERT INTO PHYSICIAN_FACILITY_LIST
	(PhysicianID, FacilityID)
VALUES
('4','7'),
('14','1'),
('13','8'),
('12','7'),
('13','3'),
('1','3'),
('12','1'),
('8','1'),
('2','6'),
('9','7'),
('3','1'),
('8','7'),
('3','6'),
('5','2'),
('2','3'),
('10','3'),
('1','1'),
('3','5'),
('11','3'),
('5','4'),
('14','7'),
('5','7'),
('11','7'),
('7','1'),
('6','1')
GO

/*INSERT INTO - 6*/
INSERT INTO FACILITY_ADDRESS_LIST
	(FacilityID, AddressID)
VALUES
	('1', '2'),
	('2', '9'),
	('2', '3'),
	('3', '8'),
	('4', '8'),
	('5', '7'),
	('6', '1'),
	('7', '4'),
	('7', '6'),
	('8', '5'),
	('9', '9')
GO

/*INSERT INTO -7*/

INSERT INTO PBG
	(PBGName)
VALUES 
	('Catholic Health Partners'),
	('CareSense'),
	('Practice True'),
	('Southern Hills'),
	('EasternShore')
GO

/*INSERT INTO - 8*/
INSERT INTO PHYSICIAN_PBG_LIST
	(PhysicianID, PBGID)
VALUES
	('13','4'),
('2','2'),
('1','4'),
('2','3'),
('1','4'),
('9','4'),
('4','4'),
('11','2'),
('10','5'),
('14','3'),
('10','4'),
('7','1'),
('8','3'),
('10','5'),
('13','3'),
('3','1'),
('1','2'),
('5','3'),
('3','3'),
('1','5'),
('2','2'),
('7','2'),
('13','3'),
('12','2'),
('6','1'),
('5','1')
GO

/*INSERT INTO - 9*/

INSERT INTO IDN
	(IDNName)
VALUES
	('Lord''s Health System'),
	('Vision Health'),
	('Grandview Healthcare'),
	('Einstein'),
	('West Falls Healthcare'),
	('Burbank Medicine')
GO

/*INSERT INTO - 10*/
INSERT INTO PHYSICIAN_PBG_LIST
	(PhysicianID, PBGID)
VALUES
	('1','3'),
	('2','4'),
	('2','5'),
	('4','1'),
	('5','2'),
	('7','5'),
	('8','1'),
	('10','4'),
	('10','3'),
	('13','1'),
	('14','2'),
	('14','3')
GO


SELECT dbo.PhysicianIDLookup ('William', 'Holt', 'III')
/*Added Suffix to Physicians Table*/
ALTER TABLE PHYSICIANS 
ADD Suffix varchar(4);

/*Additional physicians added*/
INSERT INTO PHYSICIANS 
	(FirstName, MiddleInitial, LastName)
VALUES
	('William', 'F', 'Holt'),
	('Nicholas', '', 'Woody')



INSERT INTO PHYSICIAN_IDN_LIST
	(PhysicianID, IDNID)
VALUES
	('3','2'),
	('5','4'),
	('6','3'),
	('9','1'),
	('9','6'),
	('11','5'),
	('12','5'),
	('13','1'),
	('13','5'),
	('14','6')

SELECT * FROM PHYSICIAN_ADDRESS_LIST
SELECT * FROM FACILITY_PBG_LIST
SELECT * FROM FACILITY_IDN_LIST
SELECT * FROM FACILITY_GPO_LIST
SELECT * FROM PHYSICIAN_GPO_LIST
SELECT * FROM GPO

SELECT *
FROM PHYSICIAN_DATES

SELECT *
FROM DATES
ORDER BY DATEID

INSERT INTO DATES
	(Months, Years)
VALUES
('December','2015'),
('January','2016'),
('Febaruary','2016'),
('March','2016'),
('April','2016'),
('May','2016'),
('June','2016'),
('July','2016'),
('August','2016'),
('September','2016'),
('Ocotober','2016'),
('November','2016'),
('December','2016'),
('January','2017'),
('Febaruary','2017'),
('March','2017'),
('April','2017'),
('May','2017'),
('June','2017'),
('July','2017'),
('August','2017'),
('September','2017'),
('Ocotober','2017'),
('November','2017'),
('December','2017'),
('January','2018'),
('Febaruary','2018'),
('March','2018'),
('April','2018'),
('May','2018'),
('June','2018'),
('July','2018'),
('August','2018'),
('September','2018'),
('Ocotober','2018'),
('November','2018'),
('December','2018')

INSERT INTO PHYSICIAN_DATES
	(PhysicianId, DateID)
VALUES
	('1','1'),
	('2','1'),
	('3','3'),
	('4','3'),
	('5','4'),
	('6','4'),
	('1','4'),
	('2','4'),
	('7','5'),
	('8','5'),
	('3','6'),
	('4','6'),
	('2','7'),
	('3','8'),
	('9','10'),
	('5','10'),
	('10','12'),
	('11','12'),
	('1','13'),
	('8','13'),
	('9','14'),
	('10','15'),
	('11','15'),
	('6','16'),
	('8','19'),
	('9','19'),
	('4','20'),
	('12','21'),
	('13','22'),
	('14','22')


/*CREATE VIEW OF DATES WHEN PHYSICIANS ADDED*/
SELECT P.FirstName, P.LastName, D.Months, D.Years
FROM PHYSICIANS AS P
JOIN PHYSICIAN_DATES AS PD ON P.PhysicianID = PD.PhysicianId
JOIN DATES AS D ON PD.DateID = D.DateID

	
SELECT *
FROM PBG



INSERT INTO PBG_DATES
	(PBGID, DateID)
VALUES 
	('1','3'),
	('2', '4'),
	('3','1'),
	('4', '20'),
	('5','10')




CREATE TABLE IDN_DATES (
	IDNDatesID int identity PRIMARY KEY,
	IDNID int not null FOREIGN KEY REFERENCES IDN(IDNID),
	DateID int not null FOREIGN KEY REFERENCES DATES(DateID)
)



SELECT P.FirstName, P.LastName, D.Months, D.Years
FROM PHYSICIANS AS P
JOIN PHYSICIAN_DATES AS PD ON P.PhysicianID = PD.PhysicianId
JOIN DATES AS D ON PD.DateID = D.DateID

/*CREATE VIEW OF PHYSICIAN AND IDN NAMES*/
SELECT P.FirstName, P.LastName, IDN.IDNName
FROM  PHYSICIANS AS P
JOIN PHYSICIAN_IDN_LIST AS PIL ON P.PhysicianID = PIL.PhysicianID
JOIN IDN ON PIL.IDNID = IDN.IDNID

SELECT *
FROM IDN


INSERT INTO IDN_DATES
	(IDNID, DateID)
VALUES 
	('1','10'),
	('2', '1'),
	('3','4'),
	('4', '4'),
	('5','12'),
	('6','10')

SELECT *
FROM  FACILITY_DATES

SELECT P.FirstName, P.LastName, F.FacilityName
FROM PHYSICIANS AS P
JOIN PHYSICIAN_FACILITY_LIST AS PFL ON P.PhysicianID = PFL.PhysicianID
JOIN FACILITY AS F ON PFL.FacilityID = F.FacilityID


SELECT *
FROM FACILITY

INSERT INTO FACILITY_DATES
	(FacilityID, DateID)
VALUES
	('1','1'),
	('2','4'),
	('3','3'),
	('4','4'),
	('5','1'),
	('6','1'),
	('7','1'),
	('8','10')

INSERT INTO CLASS_OF_TRADE
	(COTName)
VALUES
	('Family Medicine'),
	('Pediatrics'),
	('Oncology'),
	('Home Health'),
	('Med Surg'),
	('Cardiology')

EXEC sp_rename 'CLASS_OF_TRADE', 'SPECIALTY'

DROP TABLE FACILITY_COT

EXEC sp_rename 'PHYSICIAN_COT', 'PHYSICIAN_SPECIALTY'

EXEC sp_RENAME 'PHYSICIAN_SPECIALTY.COTID' , 'SpecialtyID', 'COLUMN'

EXEC sp_RENAME 'PHYSICIAN_SPECIALTY.PhysicianCOTID' , 'PhysicianSpecialtyID', 'COLUMN'

SELECT *
FROM PHYSICIAN_SPECIALTY

DROP TABLE PHYSICIAN_SPECIALTY

SELECT *
FROM SPECIALTY

SELECT *
FROM PHYSICIANS

INSERT INTO PHYSICIAN_SPECIALTY
	(PhysicianID, SpecialtyID)
VALUES
	('1','1'),
	('2','2'),
	('3','2'),
	('4','2'),
	('4','3'),
	('5','2'),
	('6','2'),
	('7','6'),
	('8','1'),
	('9','4'),
	('10','2'),
	('10','5'),
	('11','1'),
	('11','2'),
	('12','4'),
	('13','5'),
	('14','2')

SELECT *
FROM PHYSICIAN_DATES

SELECT *
FROM PHYSICIAN_PBG_LIST

SELECT PBGNAME, COUNT(PBGNAME) PBG_COUNT 
	FROM PBG
	JOIN PHYSICIAN_PBG_LIST ON PBG.PBGID = PHYSICIAN_PBG_LIST.PBGID
	JOIN PHYSICIANS ON PHYSICIAN_PBG_LIST.PhysicianID = PHYSICIANS.PhysicianID
	GROUP BY PBGNAME
