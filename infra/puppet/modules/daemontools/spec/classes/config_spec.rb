require 'spec_helper'

describe 'daemontools::config' do
  it { should contain_file('/etc/service') }
end
