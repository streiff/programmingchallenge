package com.rontologist.solium.romannumerals.server.numeral.unary;

import com.rontologist.solium.romannumerals.server.numeral.Numeral;

public class UnaryNumeral implements Numeral {
    private static final UnaryNumeral INSTANCE = new UnaryNumeral();
    private static final int VALUE = 1;
    private static final String REPRESENTATION = "1";

    private UnaryNumeral() { }

    public static UnaryNumeral getInstance() {
        return INSTANCE;
    }

    public int getValue() {
        return VALUE;
    }

    public String getRepresentation() {
        return REPRESENTATION;
    }
}