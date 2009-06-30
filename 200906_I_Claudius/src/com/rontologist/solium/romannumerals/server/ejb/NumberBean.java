package com.rontologist.solium.romannumerals.server.ejb;

import com.rontologist.solium.romannumerals.common.ejb.NumberRemote;
import com.rontologist.solium.romannumerals.server.converter.NumberConverter;
import com.rontologist.solium.romannumerals.server.converter.NumberConverterFactory;
import com.rontologist.solium.romannumerals.server.dao.NumberDAO;
import com.rontologist.solium.romannumerals.server.dao.NumberDAOFactory;
import com.rontologist.solium.romannumerals.server.number.RomanNumber;
import com.rontologist.solium.romannumerals.server.number.UnaryNumber;

import javax.ejb.EJBException;
import javax.ejb.Stateless;
import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.SQLException;

@Stateless (mappedName = "ejb/NumberBean")
public class NumberBean implements NumberRemote {
    private static final String DATASOURCE_NAME = "java:comp/env/jdbc/hsqlDataSource";
    private static DataSource dataSource;

    public RomanNumber getRomanNumber(String s) {
        int decimalNumber = Integer.valueOf(s);
	NumberDAO dao = NumberDAOFactory.getInstance().getNumberDAO(getConnection());

	UnaryNumber unaryNumber;
	try {
	    unaryNumber = dao.getUnaryNumber(decimalNumber);
	} catch (SQLException e) {
	    throw new EJBException("Data error", e);
	}

	NumberConverter<UnaryNumber, RomanNumber> converter = NumberConverterFactory.getInstance().getConverter(UnaryNumber.class, RomanNumber.class);
	return converter.convert(unaryNumber);
    }
    
    private Connection getConnection() {
	try {
	    if (dataSource == null) {
		dataSource = (javax.sql.DataSource) new javax.naming.InitialContext().lookup(DATASOURCE_NAME);
	    }
	    return dataSource.getConnection();
	} catch (Exception e) {
	    throw new EJBException(e.getMessage());
	}
    }
}