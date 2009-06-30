package com.rontologist.solium.romannumerals.server.dao.hsql;

import com.rontologist.solium.romannumerals.server.dao.BaseDAO;

import java.sql.Connection;

public abstract class BaseHSQLDAO extends BaseDAO {
    protected BaseHSQLDAO(Connection connection) {
	super(connection);
    }
}