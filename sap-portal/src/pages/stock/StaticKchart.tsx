import React, { useState, useEffect, useRef } from 'react';
import { Alert, Button, Card, message, Space, notification } from 'antd';
import { PageContainer } from '@ant-design/pro-layout';
import { ECharts } from 'echarts';
import * as echarts from 'echarts';
import { PlayCircleOutlined } from '@ant-design/icons';
import { once } from '@/services/eastx-sap/stock';

type Candle = { 
    datetime: string;
    open: number;
    high: number;
    low: number;
    close: number;
  };

type Bar = { 
    signal: number;
    candle: Candle;
  };

/**
 * 更新节点
 *
 * @param fields
 */
 const handleStock = async () : Promise<[any, any]> => {
    const hide = message.loading('正在请求');

    const [err, data] = await once({});

    hide();

    return [err, data];
};

const StaticKchart: React.FC = () => {
    //
    const [data, setData] = useState('{"command":"INIT"}');

    //
    const chartRef = useRef<HTMLDivElement|null>(null);

    //
    const theChart = useRef<ECharts|null>(null)

    //['2021-12-12','2021-12-13']
    const xData = useRef<string[]>([]);

    //[[1,2,3,4],[5,6,7,8]]
    const sData = useRef<Array<number[]>>([[]]);

    useEffect(() => {
        if(!chartRef?.current) {
            chartRef.current = document.getElementById('chart') as HTMLDivElement;
        }

        // 基于准备好的dom，初始化echarts实例
        theChart.current = echarts.init(chartRef.current);
    },[]);

    useEffect(() => {
        let dataObj = JSON.parse(data);
        if(dataObj?.command == 'DAT') {
            let barList = dataObj.data;
            barList.forEach((bar: Bar) => {
                let candleData = bar.candle;
                xData.current.push(candleData.datetime);
                sData.current.push([candleData.open,candleData.high,candleData.low,candleData.close]);
            });

            // 绘制图表
            theChart.current?.setOption(getOption());
        } else if (dataObj?.command == 'END') {
            console.info('END');
        } else if (dataObj?.command == 'RESET') {
            console.info('RESET');
            xData.current?.splice(0, xData.current?.length);
            sData.current?.splice(0, sData.current?.length);

            // 绘制图表
            theChart.current?.setOption(getOption());
        } else if (dataObj?.command == 'INIT') {
            console.info('INIT');
        } else {
            console.info('UNKNOWN COMMAND:' + dataObj?.command);
        }

    },[data]);
    
    const getOption = () => {
        const option = {
            xAxis: {
                data: xData.current
            },
            yAxis: {},
            dataZoom: [
                {
                    type: 'inside',
                    start: 50,
                    end: 100
                },
                {
                    show: true,
                    type: 'slider',
                    top: '90%',
                    start: 50,
                    end: 100
                }
            ],
            series: [{
                type: 'k',
                data: sData.current
            }]
        };

        return option;
    }
  
    return (
        <PageContainer>
            <Card>
                <Space>
                    <Button type="primary" 
                        icon={<PlayCircleOutlined />} 
                        onClick={async (value) => {
                 
                            const [err, data] = await handleStock();
                            if(data) {
                                console.info("success");
                                console.info(data);
                            } else {
                                console.debug(err);
                            }
 
                    }}>Start</Button>
                    <Button type="primary" 
                        icon={<PlayCircleOutlined />} 
                        onClick={(value) => {
                            
                            let promise = new Promise((resolve, reject) => {

                                console.info("1 =====> promise");

                                setTimeout(() => {
                                    console.info("3 =====> ajax");
                                    reject('error');
                                },3000);

                                console.log('2 =====> 执行console');
                            });
                            
                            promise.then((value) => {
                                console.info("then 4 =====> " + value);
                            }).catch((reason)=>{
                                console.info("catch 4 =====>" + reason);
                            });

                            console.info("====== Test Promise ======");
                    }}>Promise</Button>

                    <Button type="primary" 
                        icon={<PlayCircleOutlined />} 
                        onClick={async (value) => {
                            
                           
                    }}>Await</Button>
                </Space>
            </Card>
            <Card>
                <Alert 
                    message={data}
                    type='success'
                    showIcon
                    banner
                    style={{
                        margin: -12,
                        marginBottom: 48,
                    }}
                />
            </Card>
            <Card>
                <div ref={chartRef} id="chart" style={{ width: 800, height: 700 }}></div>
            </Card>
        </PageContainer>
    );
};

export default StaticKchart;