package com.rontologist.solium.romannumerals.server.numeral.roman;

public class CD extends AbstractRomanNumeral {
    private static final CD INSTANCE = new CD();
    private static final int VALUE = 400;

    private CD() { }

    public static CD getInstance() {
        return INSTANCE;
    }

    public int getValue() {
        return VALUE;
    }
}