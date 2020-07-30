use std::{
    fmt::{Arguments as FmtArguments},
    io::{Write, Result as IoResult},
    mem::{self},
};

use crate::{
    input::{OutputStream},
};

#[macro_export]
macro_rules! logger_format {
    ($md: ident, $msg: ident { $($rules: tt)+ }) => {
        |__cb_cb, $msg, $md| { logger_format!(impl __cb_cb $($rules)+) }
    };
    (_, $msg: ident { $($rules: tt)+ }) => {
        |__cb_cb, $msg, _| { logger_format!(impl __cb_cb $($rules)+) }
    };
    ($md: ident, _ { $($rules: tt)+ }) => {
        |__cb_cb, _, $md| { logger_format!(impl __cb_cb $($rules)+) }
    };
    (_, _ { $($rules: tt)+ }) => {
        |__cb_cb, _, _| { logger_format!(impl __cb_cb $($rules)+) }
    };
    (impl $cb: ident $cond: expr => ($($arg: tt)+) $($tail: tt)+) => {
        if $cond {
            $cb.finish(format_args!($($arg)+))
        } else {
            logger_format!(impl $cb $($tail)+)
        }
    };
    (impl $cb: ident _ => ($($arg: tt)+)) => {
        $cb.finish(format_args!($($arg)+))
    };
}

#[repr(u8)]
#[derive(Copy, Clone, Debug)]
pub enum LoggerLevel {
    Trace = 2,
    Debug = 4,
    Info = 6,
    Warning = 8,
    Error = 10,
    Output = 12,
}

#[repr(u8)]
#[derive(Copy, Clone, Debug)]
pub enum LoggerThreshold {
    Trace = 1,
    Debug = 3,
    Info = 5,
    Warning = 7,
    Error = 9,
    Output = 11,
    Nothing = 13,
}
impl LoggerThreshold {
    fn admit(self, val: LoggerLevel) -> bool {
        unsafe { mem::transmute::<LoggerThreshold, u8>(self) < mem::transmute::<LoggerLevel, u8>(val) }
    }
    fn restrict(self, val: LoggerThreshold) -> LoggerThreshold {
        unsafe { mem::transmute::<u8, LoggerThreshold>(mem::transmute::<LoggerThreshold, u8>(self).max(mem::transmute::<LoggerThreshold,u8>(val))) }
    }
    fn null(self) -> bool {
        match self {
            LoggerThreshold::Nothing => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct LoggerId {
    val: u8
}
impl LoggerId {
    fn new(val: u8) -> LoggerId {
        LoggerId { val: val }
    }
}

pub struct LoggerCallback<'a: 'b, 'b> {
    rf: &'b mut OutputStream<'a>
}
impl<'a: 'b, 'b> LoggerCallback<'a, 'b> {
    pub fn finish(self, args: FmtArguments) {
        write!(self.rf, "{}", args).expect("Logger I/O error when writing to buffer.");
    }
}

pub struct LogMetadata {
    level: LoggerLevel,
    target: &'static str,
}
impl LogMetadata {
    pub fn level(&self) -> LoggerLevel {
        self.level
    }
    pub fn target(&self) -> &str {
        self.target
    }
}

pub struct NullWriter;
impl Write for NullWriter {
    fn write(&mut self, buf: &[u8]) -> IoResult<usize> {
        Ok(buf.len())
    }
    fn flush(&mut self) -> IoResult<()> {
        Ok(())
    }
}

pub struct LoggerSlot<'a> {
    sink: OutputStream<'a>,
    format: Box<dyn 'static + Fn(LoggerCallback, &FmtArguments, &LogMetadata)>,
    level: LoggerThreshold,
}
impl<'a> LoggerSlot<'a> {
    pub fn new<F: 'static + Fn(LoggerCallback, &FmtArguments, &LogMetadata)>(wt: OutputStream<'a>, level: LoggerThreshold, format: F) -> LoggerSlot<'a> {
        LoggerSlot::<'a> {
            sink: wt,
            format: Box::new(format),
            level: level,
        }
    }
    pub fn null() -> LoggerSlot<'a> {
        LoggerSlot::<'a>::new(OutputStream::<'a>::new(NullWriter), LoggerThreshold::Nothing, logger_format!(_ , _ {
            _ => ("")
        }))
    }
}
impl<'a> Default for LoggerSlot<'a> {
    fn default() -> LoggerSlot<'a> {
        LoggerSlot::<'a>::null()
    }
}

pub struct Logger<'a> {
    slots: [LoggerSlot<'a>; 16],
    level: LoggerThreshold,
}
impl<'a> Logger<'a> {
    pub fn new(level: LoggerThreshold) -> Logger<'a> {
        Logger::<'a> {
            slots: Default::default(),
            level: level,
        }
    }
    pub fn insert(&mut self, mut slot: LoggerSlot<'a>) -> LoggerId {
        for n in 0usize..16usize {
            if self.slots[n].level.null() {
                slot.level = self.level.restrict(slot.level);
                self.slots[n] = slot;
                return LoggerId::new(n as u8)
            }
        }
        panic!("Logger capacity exceeded.")
    }
    pub fn remove(&mut self, id: LoggerId) -> Option<()> {
        let index = id.val as usize;
        if self.slots[index].level.null() {
            self.slots[index] = LoggerSlot::<'_>::null();
            Some(())
        } else {
            None
        }
    }
    pub fn flush(&mut self, id: LoggerId) {
        let rf = unsafe { self.slots.get_unchecked_mut(id.val as usize) };
        rf.sink.flush().expect("Logger I/O error when flushing buffer.");
    }
    pub fn handle<'b>(&'b mut self, id: LoggerId) -> LoggerHandle<'b, 'a> {
        LoggerHandle::<'b, 'a> {
            logger: self,
            id: id,
        }
    }
}

pub struct LoggerHandle<'a, 'b: 'a> {
    logger: &'a mut Logger<'b>,
    id: LoggerId,
}
impl<'a, 'b: 'a> LoggerHandle<'a, 'b> {
    pub fn log(self, level: LoggerLevel, target: &'static str, args: &FmtArguments) {
        let index = self.id.val as usize;
        if self.logger.slots[index].level.admit(level) {
            let sink = unsafe { self.logger.slots.get_unchecked_mut(self.id.val as usize) };
            let md = LogMetadata {
                level: level,
                target: target,
            };
            let cb = LoggerCallback::<'_, '_> {
                rf: &mut sink.sink,
            };
            (sink.format)(cb, args, &md);
        }
    }
}

#[macro_export]
macro_rules! log_trace {
    ($logger: expr; $($args: tt)+) => {
        $logger.log(LoggerLevel::Trace, "", &format_args!($($args)+))
    };
    ($logger: expr, target: $target: expr; $($args: tt)+) => {
        $logger.log(LoggerLevel::Trace, $target, &format_args!($($args)+))
    };
}

#[macro_export]
macro_rules! log_debug {
    ($logger: expr; $($args: tt)+) => {
        $logger.log(LoggerLevel::Debug, "", &format_args!($($args)+))
    };
    ($logger: expr, target: $target: expr; $($args: tt)+) => {
        $logger.log(LoggerLevel::Debug, $target, &format_args!($($args)+))
    };
}

#[macro_export]
macro_rules! log_info {
    ($logger: expr; $($args: tt)+) => {
        $logger.log(LoggerLevel::Info, "", &format_args!($($args)+))
    };
    ($logger: expr, target: $target: expr; $($args: tt)+) => {
        $logger.log(LoggerLevel::Info, $target, &format_args!($($args)+))
    };
}

#[macro_export]
macro_rules! log_warning {
    ($logger: expr; $($args: tt)+) => {
        $logger.log(LoggerLevel::Warning, "", &format_args!($($args)+))
    };
    ($logger: expr, target: $target: expr; $($args: tt)+) => {
        $logger.log(LoggerLevel::Warning, $target, &format_args!($($args)+))
    };
}

#[macro_export]
macro_rules! log_error {
    ($logger: expr; $($args: tt)+) => {
        $logger.log(LoggerLevel::Error, "", &format_args!($($args)+))
    };
    ($logger: expr, target: $target: expr; $($args: tt)+) => {
        $logger.log(LoggerLevel::Error, $target, &format_args!($($args)+))
    };
}

#[macro_export]
macro_rules! log_output {
    ($logger: expr; $($args: tt)+) => {
        $logger.log(LoggerLevel::Output, "", &format_args!($($args)+))
    };
    ($logger: expr, target: $target: expr; $($args: tt)+) => {
        $logger.log(LoggerLevel::Output, $target, &format_args!($($args)+))
    };
}