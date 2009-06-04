package com.rontologist.solium.romannumerals.server.converter;

import com.rontologist.solium.romannumerals.server.number.Number;
import com.rontologist.solium.romannumerals.server.number.UnaryNumber;
import com.rontologist.solium.romannumerals.server.number.RomanNumber;

public class NumberConverterFactory {
    private static final NumberConverterFactory INSTANCE = new NumberConverterFactory();

    private NumberConverterFactory() {}

    public static NumberConverterFactory getInstance() {
        return INSTANCE;
    }


    public <T extends Number, U extends Number>
    NumberConverter getConverter(Class<T> src, Class<U> dest) {
        if (src.equals(UnaryNumber.class) && dest.equals(RomanNumber.class)) {
            return new UnaryToRomanNumberConverter();
        } else {
            throw new RuntimeException("Don't know how to convert between " +
                src.getName() + " and " + dest.getName());
        }
    }

}