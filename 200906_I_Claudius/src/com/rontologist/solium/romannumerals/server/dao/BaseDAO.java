package com.rontologist.solium.romannumerals.server.dao;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;

public abstract class BaseDAO {
    private Connection connection;

    public BaseDAO(Connection connection) {
	this.connection = connection;
    }

    protected Connection getConnection() {
	return connection;
    }

    protected void close(Statement stmt) {
	if (stmt != null) {
	    try {
		stmt.close();
	    } catch (SQLException e) {
		// swallow
	    }
	}
    }

    protected void close(ResultSet rs) {
	if (rs != null) {
	    try {
		rs.close();
	    } catch (SQLException e) {
		// swallow
	    }
	}
    }
}
