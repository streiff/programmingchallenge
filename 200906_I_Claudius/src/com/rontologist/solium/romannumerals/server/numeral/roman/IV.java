package com.rontologist.solium.romannumerals.server.numeral.roman;

public class IV extends AbstractRomanNumeral {
    private static final IV INSTANCE = new IV();
    private static final int VALUE = 4;

    private IV() { }

    public static IV getInstance() {
        return INSTANCE;
    }

    public int getValue() {
        return VALUE;
    }
}