package com.rontologist.solium.romannumerals.server.numeral.roman;

public class I extends AbstractRomanNumeral {
    private static final I INSTANCE = new I();
    private static final int VALUE = 1;

    private I() { }

    public static I getInstance() {
        return INSTANCE;
    }

    public int getValue() {
        return VALUE;
    }
}