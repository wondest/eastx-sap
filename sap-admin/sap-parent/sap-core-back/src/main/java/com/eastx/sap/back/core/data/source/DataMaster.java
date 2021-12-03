package com.eastx.sap.back.core.data.source;

/**
 * @ClassName DataMaster
 * @Description: TODO
 * @Author Tender
 * @Time 2021/5/30 10:16
 * @Version 1.0
 * @Since 1.8
 **/
public class DataMaster {

//    /**
//     * make and registry the source
//     * @param upsource
//     * @return
//     */
//    public static DataSource makeChainedEvent(DataSource upsource) {
//        class ChainedEventSource extends AbstractDataSource.ChainedEvent {
//            ChainedEventSource(DataSource upsource) {
//                super("Chain", upsource);
//            }
//        }
//
//        DataSource newSource = new ChainedEventSource(upsource);
//        upsource.registry(newSource);
//        return newSource;
//    }

    /**
     *
     * @param fileName
     * @return
     */
    public static DataSource makeCsvEvent(String fileName) {
        class EventEnableSource extends AbstractDataSource.Proxy {
            EventEnableSource(String fileName) {
                super("CsvEvent", new FlatFileDataSource(fileName));
            }
        }

        return new EventEnableSource(fileName);
    }

    /**
     *
     * @param fileName
     * @return
     */
    public static DataSource makeCsvData(String fileName) {
        return new FlatFileDataSource(fileName);
    }
}
