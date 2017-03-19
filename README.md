# simple-sql-parser

This is an SQL Parser written in Peg. The Peg library used is [pointlander/peg](https://github.com/pointlander/peg)

This grammar handles only three statements and does not support any subqueries

1. Create
2. Insert
3. Select
4. Drop
5. Delete


#### Note:

The table names in all the queries must  be qualified with a keyspace like `keyspace.tablename`

The CREATE is not full featured. It does not support any datatypes or the Primary Key.
The grammar returns the first column as a Partition Key because this was written to be used by CQL like system.

The DELETE is not fully featured. It accepts only one WHERE condition

The INSERT, SELECT, DROP are standard. 

For more info, look at the test file.
