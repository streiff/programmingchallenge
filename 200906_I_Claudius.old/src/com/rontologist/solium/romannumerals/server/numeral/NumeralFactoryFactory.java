package com.rontologist.solium.romannumerals.server.numeral;

import com.rontologist.solium.romannumerals.server.numeral.roman.RomanNumeral;
import com.rontologist.solium.romannumerals.server.numeral.roman.RomanNumeralFactory;

import java.util.Map;
import java.util.HashMap;
import java.util.Collections;

public class NumeralFactoryFactory {
    private static final NumeralFactoryFactory INSTANCE = new NumeralFactoryFactory();

    private Map<Class<? extends Numeral>, Class<? extends NumeralFactory>> factoryLookupMap;

    private NumeralFactoryFactory() {
        factoryLookupMap = new HashMap<Class<? extends Numeral>, Class<? extends NumeralFactory>>();
        factoryLookupMap.put(RomanNumeral.class, RomanNumeralFactory.class);
        factoryLookupMap = Collections.unmodifiableMap(factoryLookupMap);

    }

    public static NumeralFactoryFactory getInstance() {
        return INSTANCE;
    }

    @SuppressWarnings({"unchecked"})
    public <T extends Numeral> NumeralFactory<T> createFactory(Class<T> numeralClass) {
        Class<? extends NumeralFactory> clazz;
        if (factoryLookupMap.containsKey(numeralClass)) {
            clazz = factoryLookupMap.get(numeralClass);
        } else {
            throw new RuntimeException("Error: no numeral factory for " + numeralClass.getName());
        }

        try {
            Object classInstance = clazz.getMethod("getInstance").invoke(null);

            if (classInstance instanceof NumeralFactory) {
                return (NumeralFactory<T>) classInstance;
            } else {
                throw new RuntimeException("Unexcepted object back when getting factory: " + classInstance.getClass().getName());
            }
        } catch (Exception e) {
            throw new RuntimeException("Error creating class: " + clazz.getName(), e);
        }
    }
}
