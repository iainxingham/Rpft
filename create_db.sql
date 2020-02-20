/*-- !preview conn=DBI::dbConnect(RSQLite::SQLite())*/
/*  Instructions to create the tables for the database
 *
 *  Produce the database with
 *
 *		cat create_db.sql | sqlite3 pfts.sqlite
 */
CREATE TABLE patient (
	id INTEGER NOT NULL, 
	fname VARCHAR(100), 
	lname VARCHAR(100), 
	rxr VARCHAR(10), 
	nhs VARCHAR(15), 
	dob DATE, 
	PRIMARY KEY (id), 
	UNIQUE (rxr)
)

CREATE TABLE studies (
  id INTEGER NOT NULL,
  tag VARCHAR(10),
  description VARCHAR(50),

  PRIMARY KEY (id)
)

CREATE TABLE datasource (
  id INTEGER NOT NULL,
  import_date DATE,
  source_file VARCHAR(200)
  subject_id INTEGER,
  study_type INTEGER,
  
  PRIMARY KEY (id),
  FOREIGN KEY (subject_id) REFERENCES patient (id),
  FOREIGN KEY (study_type) REFERENCES studies (id) 
)

CREATE TABLE spirometry (
	id INTEGER NOT NULL, 
	subject_id INTEGER, 
	study_date DATE, 
	fev1_pre FLOAT, 
	fev1_pre_pred FLOAT, 
	fev1_pre_percent_pred FLOAT, 
	"fev1_pre_SR" FLOAT, 
	fvc_pre FLOAT, 
	fvc_pre_pred FLOAT, 
	fvc_pre_percent_pred FLOAT, 
	"fvc_pre_SR" FLOAT, 
	fev1_post FLOAT, 
	fev1_post_pred FLOAT, 
	fev1_post_percent_pred FLOAT, 
	"fev1_post_SR" FLOAT, 
	fvc_post FLOAT, 
	fvc_post_pred FLOAT, 
	fvc_post_percent_pred FLOAT, 
	"fvc_post_SR" FLOAT, 
	fev1_percent_change FLOAT, 
	fev1_pred FLOAT, 
	fvc_percent_change FLOAT, 
	fvc_pred FLOAT,
	source_id INTEGER,
	PRIMARY KEY (id), 
	FOREIGN KEY (subject_id) REFERENCES patient (id),
	FOREIGN KEY (source_id) REFERENCES datasource (id)
)

CREATE TABLE lungfunc (
	id INTEGER NOT NULL, 
	subject_id INTEGER, 
	spiro_id INTEGER, 
	study_date DATE, 
	tlco FLOAT, 
	tlco_pred FLOAT, 
	tlco_percent_pred FLOAT, 
	"tlco_SR" FLOAT, 
	vasb FLOAT, 
	vasb_pred FLOAT, 
	vasb_percent_pred FLOAT, 
	kco FLOAT, 
	kco_pred FLOAT, 
	kco_percent_pred FLOAT, 
	frc FLOAT, 
	frc_pred FLOAT, 
	frc_percent_pred FLOAT, 
	"frc_SR" FLOAT, 
	vc FLOAT, 
	vc_pred FLOAT, 
	vc_percent_pred FLOAT, 
	"vc_SR" FLOAT, 
	tlc FLOAT, 
	tlc_pred FLOAT, 
	tlc_percent_pred FLOAT, 
	"tlc_SR" FLOAT, 
	rv FLOAT, 
	rv_pred FLOAT, 
	rv_percent_pred FLOAT, 
	"rv_SR" FLOAT, 
	tlcrv FLOAT, 
	tlcrv_pred FLOAT, 
	tlcrv_percent_pred FLOAT, 
	"tlcrv_SR" FLOAT,
	source_id INTEGER,
	PRIMARY KEY (id), 
	FOREIGN KEY (spiro_id) REFERENCES spirometry (id), 
	FOREIGN KEY (subject_id) REFERENCES patient (id),
	FOREIGN KEY (source_id) REFERENCES datasource (id)
)

INSERT INTO "studies"("tag", "description") VALUES ("FULL_PFT", "Full pulmonary function test")
INSERT INTO "studies"("tag", "description") VALUES ("PFT_Spiro", "Spirometry (from lab PFT)")