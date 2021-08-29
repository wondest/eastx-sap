import React, { useState, useEffect, useRef } from 'react';
import Subscribe from './components/Subscribe'
import { Alert, Button, Card, message } from 'antd';
import { PageContainer } from '@ant-design/pro-layout';
import { ECharts } from 'echarts';
import * as echarts from 'echarts';
import { PlayCircleOutlined } from '@ant-design/icons';
import { flow } from '@/services/eastx-sap/stock';

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
 const handleStock = async () => {
    const hide = message.loading('正在请求');
    try {
      await flow({
      });
      hide();
  
      message.success('请求成功');
      return true;
    } catch (error) {
      hide();
      message.error('请求失败请稍后重试！');
      return false;
    }
};

const BasicKchart: React.FC = () => {
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
            let candleData = dataObj.data.candle as Candle;
            xData.current.push(candleData.datetime);
            sData.current.push([candleData.open,candleData.high,candleData.low,candleData.close]);

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
                <Subscribe url="ws://localhost:8080/api/stock" onMessage={(message) => {setData(message);}}/>
                <Button type="primary" 
                    icon={<PlayCircleOutlined />} 
                    onClick={async (value) => {
                        const success = await handleStock();
                        if (success) {
                            setData('{"command":"RESET"}');
                        }
        }}>Start</Button>
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

export default BasicKchart;