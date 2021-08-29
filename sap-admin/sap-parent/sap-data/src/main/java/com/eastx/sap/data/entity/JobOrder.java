package com.eastx.sap.data.entity;

import com.eastx.sap.data.type.ExecLockEnum;
import lombok.Data;

import javax.persistence.*;
import java.sql.Timestamp;

/**
 * @ClassName com.eastx.sap.data.entity
 * @Description: 记录并控制发起作业命令
 * @Author Tender
 * @Time 2021/8/6 23:56
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
@Entity
@Table(name = "job_order"
        ,indexes = {@Index(name = "ix_job_order_biz", columnList = "biz_date")}
        ,uniqueConstraints = {@UniqueConstraint(name = "uc_job_order_biz_job", columnNames = {"biz_date", "job_id"})})
@Data
public class JobOrder {
    @Id
    @Column(name="id", length = 20)
    String id;

    @Column(name="biz_date", nullable = false, length = 8)
    String bizDate;

    @Column(name="batch_no", nullable = false)
    Long batchNo;

    @Convert(converter = ExecLockEnum.Converter.class)
    @Column(name="lock_state", nullable = false)
    ExecLockEnum lockState;

    @Column(name="exec_id")
    Long execId;

    @Column(name="begin_time")
    Timestamp beginTime;

    @Column(name="end_time")
    Timestamp endTime;

    @Column(name="last_updated")
    Timestamp lastUpdated;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name="job_id")
    JobBase job;
}
