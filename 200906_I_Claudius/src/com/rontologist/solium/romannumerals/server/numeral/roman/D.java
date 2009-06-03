package com.rontologist.solium.romannumerals.server.numeral.roman;

public class D extends AbstractRomanNumeral {
    private static final D INSTANCE = new D();
    private static final int VALUE = 500;

    private D() { }

    public static D getInstance() {
        return INSTANCE;
    }

    public int getValue() {
        return VALUE;
    }
}