package com.rontologist.solium.romannumerals.server.dao;

import com.rontologist.solium.romannumerals.server.dao.hsql.HSQLNumberDAO;

import java.sql.Connection;

public class NumberDAOFactory {
    private static final NumberDAOFactory instance = new NumberDAOFactory();

    private NumberDAOFactory() {
    }

    public static NumberDAOFactory getInstance() {
	return instance;
    }

    public NumberDAO getNumberDAO(Connection conn) {
	return new HSQLNumberDAO(conn); 
    }
}
