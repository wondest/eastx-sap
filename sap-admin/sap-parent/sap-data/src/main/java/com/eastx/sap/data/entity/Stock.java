package com.eastx.sap.data.entity;

import com.eastx.sap.data.type.ExchangeEnum;
import lombok.Data;

import javax.persistence.*;
import java.sql.Timestamp;

/**
 * @ClassName com.eastx.spa.data.StockInfo
 * @Description: TODO
 * @Author Tender
 * @Time 2021/8/6 23:56
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
@Entity
@Table(name = "stock"
        ,uniqueConstraints = {@UniqueConstraint(name = "uc_stock", columnNames = {"exchange", "code"})})
@Data
public class Stock {
    @Id
    @GeneratedValue(strategy=GenerationType.IDENTITY)
    Long id;

    /**
     * EnumType:  ORDINAL 枚举序数  默认选项（int）。eg:TEACHER 数据库存储的是 0
     *            STRING  枚举名称       (String)。eg:TEACHER 数据库存储的是 "TEACHER"
     */
    @Convert(converter = ExchangeEnum.Converter.class)
    private ExchangeEnum exchange;

    @Column(name="code", length = 6)
    String code;

    @Column(name="company", length = 255)
    String company;

    @Column(name="last_updated", length = 6)
    Timestamp lastUpdated;
}
