package com.rontologist.solium.romannumerals.server.numeral.roman;

public class L extends AbstractRomanNumeral {
    private static final L INSTANCE = new L();
    private static final int VALUE = 50;

    private L() { }

    public static L getInstance() {
        return INSTANCE;
    }

    public int getValue() {
        return VALUE;
    }
}