--CREATE DATABASE sudoku;
BEGIN;

CREATE TABLE grid (
    id int primary key not null,
    solved boolean not null
);

CREATE TABLE cell (
    id int primary key not null,
    grid_id int not null references grid(id),
    y int not null,
    x int not null,
    real_value int,
    user_value int,
    revealed boolean not null
);

COMMIT;