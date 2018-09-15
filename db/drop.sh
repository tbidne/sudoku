#!/bin/bash

psql sudoku -f ./drop_tables.sql
psql -f ./drop_db.sql