#!/bin/bash

psql -f ./create_db.sql
psql sudoku -f ./create_tables.sql