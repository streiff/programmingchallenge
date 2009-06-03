package com.rontologist.solium.romannumerals.server.numeral.roman;

public class V extends AbstractRomanNumeral {
    private static final V INSTANCE = new V();
    private static final int VALUE = 5;

    private V() { }

    public static V getInstance() {
        return INSTANCE;
    }

    public int getValue() {
        return VALUE;
    }
}