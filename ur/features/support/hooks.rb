After do
    Process.kill(:SIGTERM, @pid) if @pid
    Process.wait(@pid) if @pid
end
