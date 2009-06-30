package com.rontologist.solium.romannumerals.server.dao;

import com.rontologist.solium.romannumerals.server.number.UnaryNumber;

import java.sql.SQLException;

public interface NumberDAO {
    UnaryNumber getUnaryNumber(int number) throws SQLException;
}
