package com.rontologist.solium.romannumerals.server.dao.hsql;

import com.rontologist.solium.romannumerals.server.dao.NumberDAO;
import com.rontologist.solium.romannumerals.server.number.UnaryNumber;
import com.rontologist.solium.romannumerals.server.numeral.unary.UnaryNumeral;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.LinkedList;
import java.util.List;

public class HSQLNumberDAO extends BaseHSQLDAO implements NumberDAO {
    public HSQLNumberDAO(Connection connection) {
	super(connection);
    }

    private static final String GET_UNARY_NUMBER_SQL =
	    "SELECT unary_num " +
	    "  FROM numerals " +
	    " WHERE dec_num = ?";
    public UnaryNumber getUnaryNumber(int number) throws SQLException {
	Connection conn = getConnection();
	PreparedStatement stmt = null;
	ResultSet rs = null;

	try {
	    stmt = conn.prepareStatement(GET_UNARY_NUMBER_SQL);
	    stmt.setInt(1, number);
	    rs = stmt.executeQuery();

	    if (rs.next()) {
		String numerals = rs.getString(1);
		List<UnaryNumeral> numeralList = new LinkedList<UnaryNumeral>();
		for (char c : numerals.toCharArray()) {
		    if (String.valueOf(c).equals(UnaryNumeral.getInstance().getRepresentation())) {
			numeralList.add(UnaryNumeral.getInstance());
		    } else {
			throw new RuntimeException("Non-unary numeral detected in database");
		    }
		}
		return new UnaryNumber(numeralList);
	    } else {
		return null;
	    }
	} finally {
	    close(rs);
	    close(stmt);
	}
    }
}