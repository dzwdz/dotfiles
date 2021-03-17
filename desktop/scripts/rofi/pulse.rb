#!/usr/bin/ruby
# switches pulseaudio sinks

sinks = `pacmd list-sinks | grep device.description`
  .scan /"(.+)"\n/

if ARGV.empty?
  sinks.each {|s| puts s}
else
  target = sinks.index ARGV
  `pacmd set-default-sink #{target}`
end
