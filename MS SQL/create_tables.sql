-- select @@VERSION;
/* The  sales.stores table includes the store’s information.
Each store has a store name, contact information such as
phone and email, and an address including street, city,
state, and zip code
--------------------------------------------------------------------
Name   : BikeStores
Link   : http://www.sqlservertutorial.net/load-sample-database/
Link   : https://stackoverflow.com/questions/43525781/ms-sql-query-if-not-exists-a-table
Version: 1.0
--------------------------------------------------------------------

*/
/*first create schemas and reference tables as 
schema.table-name
*/
-- create schemas
create schema production;
go
create schema sales;
go

create table sales.stores(
	store_id int identity (1, 1) primary key,
	store_name varchar (255) not null,
	phone varchar (25),
	email varchar (255),
	street varchar (255),
	city varchar (255),
	state varchar (255),
	zip_code varchar (5)
);

/* The sales.staffs table stores essential information of
of staffs including first name, last name and ifo like email
and phone.
If a staff no longer works for any stores, the value in 
the active column is set to zero.
*/
create table sales.staff(
	staff_id int identity (1, 1) primary key,
	first_name varchar (50) not null,
	last_name varchar (50) not null,
	email varchar (255) not null unique,
	phone varchar (50),
	active tinyint not null,
	store_id int not null,
	manager_id int,
	foreign key (store_id)
	references sales.stores(store_id)
	on delete cascade on update cascade,
	foreign key (manager_id)
	references sales.staff (staff_id)
	on delete no action on update no action
);
/*
the production.categories table stores bike categories
*/
create table production.categories (
	category_id int identity (1, 1) primary key,
	category_name varchar (255) not null
);
/* The production.products table stores the product’s
information such as name, brand, category, model year,
and list price.
*/
CREATE TABLE production.brands (
	brand_id INT IDENTITY (1, 1) PRIMARY KEY,
	brand_name VARCHAR (255) NOT NULL
);

create table production.products(
	product_id int identity (1,1) primary key,
	product_name varchar (255) not null,
	brand_id int not null,
	category_id int not null,
	model_year smallint not null,
	list_price decimal (10,2) not null,
	foreign key (category_id)
	references production.categories (category_id)
	on delete cascade on update cascade,
	foreign key (brand_id)
	references production.brands(brand_id)
	on delete cascade on update cascade
);

/* sales.customer table stores custormer info 
*/
CREATE TABLE sales.customers (
	customer_id INT IDENTITY (1, 1) PRIMARY KEY,
	first_name VARCHAR (255) NOT NULL,
	last_name VARCHAR (255) NOT NULL,
	phone VARCHAR (25),
	email VARCHAR (255) NOT NULL,
	street VARCHAR (255),
	city VARCHAR (50),
	state VARCHAR (25),
	zip_code VARCHAR (5)
);
/* slaes.orders table stores sales order's header information
*/
CREATE TABLE sales.orders (
	order_id INT IDENTITY (1, 1) PRIMARY KEY,
	customer_id INT,
	order_status tinyint NOT NULL,
	-- Order status: 1 = Pending; 2 = Processing; 3 = Rejected; 4 = Completed
	order_date DATE NOT NULL,
	required_date DATE NOT NULL,
	shipped_date DATE,
	store_id INT NOT NULL,
	staff_id INT NOT NULL,
	FOREIGN KEY (customer_id) 
        REFERENCES sales.customers (customer_id) 
        ON DELETE CASCADE ON UPDATE CASCADE,
	FOREIGN KEY (store_id) 
        REFERENCES sales.stores (store_id) 
        ON DELETE CASCADE ON UPDATE CASCADE,
	FOREIGN KEY (staff_id) 
        REFERENCES sales.staff (staff_id) 
        ON DELETE NO ACTION ON UPDATE NO ACTION
);
/* sales.order_items table stores the line items of sales
order
*/
CREATE TABLE sales.order_items(
	order_id INT,
	item_id INT,
	product_id INT NOT NULL,
	quantity INT NOT NULL,
	list_price DECIMAL (10, 2) NOT NULL,
	discount DECIMAL (4, 2) NOT NULL DEFAULT 0,
	PRIMARY KEY (order_id, item_id),
	FOREIGN KEY (order_id) 
        REFERENCES sales.orders (order_id) 
        ON DELETE CASCADE ON UPDATE CASCADE,
	FOREIGN KEY (product_id) 
        REFERENCES production.products (product_id) 
        ON DELETE CASCADE ON UPDATE CASCADE
);
/* production.stocks table stores the inventory info */
CREATE TABLE production.stocks (
	store_id INT,
	product_id INT,
	quantity INT,
	PRIMARY KEY (store_id, product_id),
	FOREIGN KEY (store_id) 
        REFERENCES sales.stores (store_id) 
        ON DELETE CASCADE ON UPDATE CASCADE,
	FOREIGN KEY (product_id) 
        REFERENCES production.products (product_id) 
        ON DELETE CASCADE ON UPDATE CASCADE
);







