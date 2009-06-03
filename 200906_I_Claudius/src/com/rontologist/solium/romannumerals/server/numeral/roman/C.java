package com.rontologist.solium.romannumerals.server.numeral.roman;

public class C extends AbstractRomanNumeral {
    private static final C INSTANCE = new C();
    private static final int VALUE = 100;

    private C() { }

    public static C getInstance() {
        return INSTANCE;
    }

    public int getValue() {
        return VALUE;
    }
}