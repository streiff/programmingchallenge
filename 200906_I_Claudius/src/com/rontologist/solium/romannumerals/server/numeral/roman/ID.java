package com.rontologist.solium.romannumerals.server.numeral.roman;

public class ID extends AbstractRomanNumeral {
    private static final ID INSTANCE = new ID();
    private static final int VALUE = 499;

    private ID() { }

    public static ID getInstance() {
        return INSTANCE;
    }

    public int getValue() {
        return VALUE;
    }
}