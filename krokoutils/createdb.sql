/* copyright Vincent Balat 2005 */
/*
To create the tables, first create the krokobase database, 
then do
psql -f createdb.sql krokobase
*/

drop table globalstore;
drop table content;
drop table users;
drop table ressources;

create table globalstore (
	key text primary key,
	value bytea);

create table content (
	content_key        serial primary key,
	data               bytea
);


create table users (
	uid             serial primary key,
	login           text unique not null,
 	password        bytea,
	real_name       text,
/*	user_creation_date timestamp,
	last_connection_date timestamp,*/
	groups          bytea
);

create table ressources (
	rid             serial primary key,
	rgroups          bytea
);

/*
create table groups (
	gid             serial primary key,
	group_name      text unique not null,
	group_description text
);
*/

/*
create table rights (
	id              serial4 primary key,
	name            text unique not null,
	description     text
);

create table categories (
	id              serial4 primary key,
	name            text unique not null,
	description     text
);
*/
/*
create table messages (
	key             serial primary key,
	title           text,
	content         text,
	syntax          bytea,
	last_author     integer, -- references users,
	last_date       timestamp,
	creation_author integer, -- references users,
	creation_date   timestamp,
	version         integer, -- version number
	previous_versions bytea, 
	sons            bytea, -- marshaled list of sons id
	big_ancester    integer
);

create table oldmessages (
	key_old         serial primary key,
	diff_title      text,
	diff_content    text,
	old_syntax      bytea,
        modif_author    text, -- references users,
        modif_date      timestamp
);




/*

/* rights for each group  */
create table groups_rights (
	group	 	int4 references groups,
	right           int4 references rigths,
	primary key (group, right)
);




------------- fin

/*******************************************
    les droit pour www-data
****************************************/

grant all on messages to "www-data";

*/
*/