package com.eastx.sap.data.entity;

import com.eastx.sap.data.type.JobTypeEnum;
import lombok.Data;
import org.hibernate.annotations.DynamicUpdate;

import javax.persistence.*;
import java.sql.Timestamp;

/**
 * @ClassName com.eastx.sap.data.entity
 * @Description: 作业定义,包含作业的名称、版本、描述、order类型
 *               (1) order类型用来区分本作业order起来的bizKey来源，例如：如果orderType是stock那么就从stock表内获取股票代码用作bizKey
 * @Author Tender
 * @Time 2021/8/6 23:56
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
@Entity
@Table(name = "job_base"
        ,indexes = {@Index(name = "ix_job_base_type", columnList = "job_type")}
        ,uniqueConstraints = {@UniqueConstraint(name = "uc_job_base_name", columnNames = {"job_name"})})
@DynamicUpdate
@Data
public class JobBase {
    @Id
    @Column(name="id", length = 20)
    String id;

    @Column(name="job_name", length = 100, nullable = false)
    String name;

    @Column(name="description", length = 128)
    String description;

    @Column(name="version", length = 10)
    String version;

    @Convert(converter = JobTypeEnum.Converter.class)
    @Column(name="job_type", nullable = false)
    JobTypeEnum jobType;

    @Column(name="last_updated")
    Timestamp lastUpdated;
}
