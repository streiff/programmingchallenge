package com.rontologist.solium.romannumerals.server.numeral.roman;

public class IL extends AbstractRomanNumeral {
    private static final IL INSTANCE = new IL();
    private static final int VALUE = 49;

    private IL() { }

    public static IL getInstance() {
        return INSTANCE;
    }

    public int getValue() {
        return VALUE;
    }
}