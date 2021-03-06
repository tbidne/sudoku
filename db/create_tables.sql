BEGIN;

CREATE TABLE grid (
    id int primary key not null,
    solved boolean not null
);

CREATE TABLE cell (
    id int primary key not null,
    grid_id int not null references grid(id),
    cCol int not null,
    cRow int not null,
    real_value int,
    user_value int,
    revealed boolean not null
);

COMMIT;